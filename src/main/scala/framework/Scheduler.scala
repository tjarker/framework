package framework

import scala.collection.mutable
import java.util.concurrent.Semaphore
import framework.Time.*

object Scheduler {

  class SimulationThread(
      val name: String,
      val t: Thread,
      scheduler: Scheduler,
      val semaphore: Semaphore
  ) {

    def wake(): Unit = semaphore.release()
    override def toString: String = name

  }

  object SimulationThread {
    def unapply(st: SimulationThread): Option[(String, Thread)] =
      Some((st.name, st.t))
  }

  def getCurrentSimCurrent(scheduler: Scheduler): SimulationThread = {
    scheduler.threads.filter(_.t == Thread.currentThread()).head
  }
}

class Scheduler(now: => AbsoluteTime, advance: AbsoluteTime => Unit) {

  import Scheduler.*

  val wakeTimes = mutable.Map[SimulationThread, AbsoluteTime]()
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
    wakeTimes(simThread) = now
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

  def advanceTime(to: AbsoluteTime): Unit = {
    advance(to)
    println(s"\n\n[Time] $to")
  }

  def scheduleNewThread(old: SimulationThread, keepAlive: Boolean = true) = {

    println(s"[Sched] scheduling new thread")
    println(s"[Sched] ${haveToRunInThisTick}")

    if (haveToRunInThisTick.isEmpty) {
      println("[Sched] No more work for this tick")
      advanceTime(nextTickToBeScheduled)
    }

    val candidates = haveToRunInThisTick

    if (candidates.isEmpty) throw new Exception("No candidates to run")

    val next = candidates.head

    if (next != old) {
      next.wake()

      println(s"[Sched] switching $old -> $next")
      if (keepAlive) old.semaphore.acquire()
    } else {
      println(s"[Sched] keeping $old running")
    }


    
  }

  def sleepUntil(until: AbsoluteTime): Unit = {

    val src = threadToSimThread(Thread.currentThread())

    println(s"[Sched] $src sleeps until ${until}")
    wakeTimes(src) = until

    scheduleNewThread(src)
  }

  def haveToRunInThisTick: Seq[SimulationThread] = {
    wakeTimes
      .filter { case (thread, time) => time == now }
      .keys
      .toSeq
  }

  def nextTickToBeScheduled: AbsoluteTime = {
    wakeTimes.values.minBy(_.fs)
  }


  def killAll(): Unit = {
    threads.foreach {
      case SimulationThread(name, t) =>
        if (name != "main") {
          println(s"[Sched] killing $name")
          t.interrupt()
        }
    }
  }

  def joinAll(): Unit = {
    threads.foreach {
      case SimulationThread(name, t) =>
        if (name != "main") {
          println(s"[Sched] joining $name")
          t.join()
        }
    }
  }
}
