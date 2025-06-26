package framework.scheduler

import java.util.concurrent.ThreadFactory
import scala.collection.mutable
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicBoolean

class ThreadedTask[T](val t: Thread) extends InternalTask[T] {
  var stopped = false
  def stop() = {
    //stopped = true
    LockSupport.park()

    while(Task.current != this) {
      LockSupport.park()
      println(s"[${Task.current}] $this woke up without a reason")
    }
    //println(s"[${Task.current}] $this is runnings")
  }
  var continue: () => Unit = () => {
    //println(s"[${Task.current}] starting $this")
    t.start()
    continue = () => {
      //stopped = false
      //println(s"[${Task.current}] waking $this ($stopped)")
      LockSupport.unpark(t)
    }
  }

  override def toString(): String = t.getName()
}

class VirtualThreadScheduler extends ThreadedScheduler(Thread.ofVirtual().name("virt-", 0L).factory().newThread)
class PlatformThreadScheduler extends ThreadedScheduler(r => new Thread(r, "plat-" + r.hashCode()))


class ThreadedScheduler(threadFactory: Runnable => Thread) extends Scheduler {

  var currentTask: ThreadedTask[?] = null
  val readyQueue = new mutable.Queue[ThreadedTask[?]]()

  val log = new mutable.ArrayBuffer[Event]

  def launchTask[T](r: => T): Task[T] = {
    var task: ThreadedTask[T] = null

    val thread = threadFactory(new Runnable {
      def run(): Unit = {
        try {
          addLog(Event.Started(task, readyQueue.toSeq))
          val res = r
          task.result = Some(TaskResult.Ok(res))
        } catch {
          case TaskCancelledException =>
            task.result = Some(TaskResult.Canceled)
          case e: Throwable =>
            task.result = Some(TaskResult.Err(e))
            task.printException(e)
            println(log.takeRight(100).mkString("\n"))
        } finally {
          task.onComplete()
          addLog(Event.Finished(task, readyQueue.toSeq))
          if (readyQueue.isEmpty) throw new IllegalStateException("No tasks to yield to at finish")
          val nextTask = readyQueue.dequeue()
          currentTask = nextTask
          nextTask.continue()
        }
      }
    })

    task = new ThreadedTask[T](thread)
    addLog(Event.Created(task, readyQueue.toSeq))
    readyQueue.enqueue(task)
    task
  }

  def suspendCurrentTask(): Unit = {
    addLog(Event.Suspend(currentTask, readyQueue.toSeq))
    if (readyQueue.isEmpty) throw new IllegalStateException("No tasks to yield to at suspend")
    val nextTask = readyQueue.dequeue()
    if (nextTask != currentTask) {
      val oldTask = currentTask
      currentTask = nextTask
      nextTask.continue()
      oldTask.stop()
      if (oldTask.cancelled) throw TaskCancelledException
      addLog(Event.Resuming(oldTask, readyQueue.toSeq))
    }
  }

  def scheduleTask(task: Task[?]): Unit = {
    addLog(Event.Schedule(currentTask, task, readyQueue.toSeq))
    readyQueue.enqueue(task.asInstanceOf[ThreadedTask[?]])
  }

  def cancelTask(task: Task[?]): Unit = {
    val threadTask = task.asInstanceOf[ThreadedTask[?]]
    threadTask.cancelled = true
    readyQueue.dequeueAll(_ == threadTask)
    readyQueue.prepend(threadTask)
  }

  def runBlocking[T](block: => T): T = {
    val thread = Thread.currentThread()
    val task = new ThreadedTask[T](thread)
    task.continue = () => LockSupport.unpark(thread)
    currentTask = task
    try {
      block
    } catch {
      case e: Throwable => 
        println(this.log.takeRight(100).mkString("\n"))
        throw e
    }
  }

  def addLog(e: Event): Unit = {
    //println(s"[$currentTask] $e")
    log += e
  }

}