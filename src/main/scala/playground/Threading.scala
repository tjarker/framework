
import scala.collection.mutable
import java.util.concurrent.Semaphore
import scala.util.Random

object Threading {

  var simulationTime: Long = 0


  trait Event {
    def evaluate(): Boolean
  }

  case class WaitSimulationTicks(n: Long) extends Event {
    val targetTick = simulationTime + n
    def evaluate(): Boolean = targetTick == simulationTime
  }

  class SimulationThread(val name: String, val t: Thread, scheduler: Scheduler, val semaphore: Semaphore) {

    def wake(): Unit = semaphore.release()
    override def toString: String = name


  }

  def getCurrentSimCurrent(scheduler: Scheduler): SimulationThread = {
    scheduler.threads.filter(_.t == Thread.currentThread()).head
  }

  class Scheduler {

    val wakeTimes = mutable.Map[SimulationThread, Long]()
    val threads = mutable.ArrayBuffer[SimulationThread]()
    val threadToSimThread = mutable.Map[Thread, SimulationThread]()

    def addMainThread() = {
      val mainThread = Thread.currentThread()
      val simThread = SimulationThread("main", mainThread, this, new Semaphore(0))
      threads += simThread
      threadToSimThread(mainThread) = simThread
    }

    def addThread(name: String)(body: => Any): Unit = {
      val sem = new Semaphore(0)
      val thread = new Thread {
        override def run(): Unit = {
          sem.acquire()
          body
          retire()
        }
      }
      thread.start()
      val simThread = SimulationThread(name, thread, this, sem)
      threads += simThread
      wakeTimes(simThread) = simulationTime
      threadToSimThread(thread) = simThread
    }

    def retire(): Unit = {
      val src = threadToSimThread(Thread.currentThread())
      threads -= src
      wakeTimes -= src
      threadToSimThread -= Thread.currentThread()

      println(s"[Sched] $src requests retirement")
      scheduleNewThread(src, false)
      println(s"[Sched] retiring $src")
    }

    def advanceTime(to: Long): Unit = {
      println(s"\n\n[Time] $to")
      simulationTime = to
    }

    def scheduleNewThread(old: SimulationThread, keepAlive: Boolean = true) = {
      if (haveToRunInThisTick.isEmpty) {
        println("[Sched] No more work for this tick")
        advanceTime(nextTickToBeScheduled)
      }

      val candidates = haveToRunInThisTick

      val next = candidates.head

      if (next != old) {
        next.wake()

        println(s"[Sched] switching $old -> $next")
        if (keepAlive) old.semaphore.acquire()
      } else {
        println(s"[Sched] keeping $old running")
      }

    }

    def stepUntil(wakeEvent: Event): Unit = {

      val src = threadToSimThread(Thread.currentThread())

      wakeEvent match {
        case WaitSimulationTicks(n) =>
          println(s"[Sched] $src sleeps for $n ticks")
          wakeTimes(src) = simulationTime + n
      }

      scheduleNewThread(src)
    }

    def haveToRunInThisTick: Seq[SimulationThread] = {
      wakeTimes.filter { case (thread, time) => time == simulationTime }.keys.toSeq
    }

    def nextTickToBeScheduled: Long = {
      wakeTimes.values.min
    }


  }

}

import Threading.*

@main def schedulerMain = {

  val n = 1000000

  val sched = Scheduler()

  // time
  val start = System.currentTimeMillis()

  sched.addMainThread()

  sched.addThread("nested") {
    println("inside nested thread")
    sched.stepUntil(WaitSimulationTicks(5))
    println("nested thread half way")
    sched.stepUntil(WaitSimulationTicks(2))
    println("nested thread done")
  }

  sched.addThread("nested2") {
    for (_ <- 0 until n) {
      sched.stepUntil(WaitSimulationTicks(Random.nextInt(200)))
    }
  }

  sched.addThread("nested3") {
    for (_ <- 0 until n) {
      sched.stepUntil(WaitSimulationTicks(Random.nextInt(200)))
    }
  }

  for (i <- 0 until 10) {
    println(s"main thread tick $i")
    sched.stepUntil(WaitSimulationTicks(2))
  }

  for (_ <- 0 until n) {
    sched.stepUntil(WaitSimulationTicks(Random.nextInt(200)))
  }

  val end = System.currentTimeMillis()

  println(s"Simulation took ${end - start} ms")
  // sim freq
  println(s"Simulation frequency: ${simulationTime.toDouble / (end - start)}")


}


@main def threadingMain = {

  val sem = new Semaphore(0)

  // Thread that goes to sleep until it is woken up
  val sleeper = new Thread {
    override def run(): Unit = {

      println("Going to sleep...")

      sem.acquire() // Sleep until woken up

      println("Woken up!")
    }
  }

  // Thread that wakes up the sleeping thread after some time
  val waker = new Thread {
    override def run(): Unit = {
      Thread.sleep(2000) // Simulate work before waking up the sleeper

      println("Waking up sleeper...")
      sem.release() // Wake up the sleeper

    }
  }

  // Start the threads
  sleeper.start()
  waker.start()

  // Join the threads to ensure main thread waits for completion
  sleeper.join()
  waker.join()
}

@main def waitCounting = {
  val numThreads = 3

  val obj = new Object

  val threads = Seq.tabulate(numThreads) { i =>
    new Thread {
      override def run(): Unit = {
        println(s"Start $i")
        for (j <- 0 until 5) {
          obj.synchronized {
            obj.wait()
            println(s"Thread $i: $j")
            obj.notify()
          }
        }
      }
    }
  }

  threads.foreach(_.start())

  Thread.sleep(1000)

  println("Start")
  obj.synchronized {
    obj.notify()
  }

  threads.foreach(_.join())

}


