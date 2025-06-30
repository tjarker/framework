package framework.coop

import framework.coop.{Task, TaskCancelledException}

import java.util.concurrent.locks.LockSupport
import scala.collection.mutable
import scala.util.{Failure, Success}

class ThreadedTask[T](val t: Thread) extends TaskImpl[T] {
  var stopped = false
  var continue: () => Unit = () => {
    t.start()
    continue = () => {
      LockSupport.unpark(t)
    }
  }

  def stop(): Unit = {
    LockSupport.park()

    while (Task.current != this) {
      LockSupport.park()
    }
  }

  override def toString: String = t.getName
}

class VirtualThreadScheduler
    extends ThreadedScheduler(
      Thread.ofVirtual().name("virt-", 0L).factory().newThread
    )
class PlatformThreadScheduler
    extends ThreadedScheduler(r => new Thread(r, "plat-" + r.hashCode()))

class ThreadedScheduler(threadFactory: Runnable => Thread) extends Scheduler {

  val readyQueue = new mutable.Queue[ThreadedTask[?]]()
  var currentTask: ThreadedTask[?] = null

  def launchTask[T](r: => T): Task[T] = {
    var task: ThreadedTask[T] = null

    val thread = threadFactory(() => {
      try {
        val res = r
        task.result = Some(Success(res))
      } catch {
        case t: TaskCancelledException =>
          task.result = Some(Failure(t))
        case e: Throwable =>
          task.result = Some(Failure(e))
          task.printException(e)
      } finally {
        task.onComplete()
        if (readyQueue.isEmpty)
          throw new IllegalStateException("No tasks to yield to at finish")
        val nextTask = readyQueue.dequeue()
        currentTask = nextTask
        nextTask.continue()
      }
    })

    task = new ThreadedTask[T](thread)
    readyQueue.enqueue(task)
    task
  }

  def suspendCurrentTask(): Unit = {
    if (readyQueue.isEmpty)
      throw new IllegalStateException("No tasks to yield to at suspend")
    val nextTask = readyQueue.dequeue()
    if (nextTask != currentTask) {
      val oldTask = currentTask
      currentTask = nextTask
      nextTask.continue()
      oldTask.stop()
      if (oldTask.cancelled) throw TaskCancelledException(oldTask.toString())
    }
  }

  def scheduleTask(task: Task[?]): Unit = {
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
        throw e
    }
  }

}
