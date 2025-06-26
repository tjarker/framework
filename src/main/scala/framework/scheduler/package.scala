package framework

import scala.collection.mutable

package object scheduler {

  def debug(msg: String): Unit = ()

  sealed trait TaskResult[+T]
  object TaskResult {
    case class Ok[+T](value: T) extends TaskResult[T]
    case class Err(exception: Throwable) extends TaskResult[Nothing]
    case object Canceled extends TaskResult[Nothing]
  }

  case object TaskCancelledException extends Exception("Task was cancelled")

  trait Task[T] {
    def await: TaskResult[T]
  }

  object Task {
    def current: Task[?] = Scheduler.current.currentTask
  }

  sealed trait Event
  object Event {
    case class Created(t: Task[?], queue: Seq[Task[?]]) extends Event
    case class Started(t: Task[?], queue: Seq[Task[?]]) extends Event
    case class Finished(t: Task[?], queue: Seq[Task[?]]) extends Event
    case class Suspend(t: Task[?], queue: Seq[Task[?]]) extends Event
    case class Schedule(a: Task[?], b: Task[?], queue: Seq[Task[?]]) extends Event
    case class Send(t: Task[?], c: Channel[?], v: Any) extends Event
    case class ReceiveSuspend(t: Task[?], c: Channel[?]) extends Event
    case class ReceiveDirect(t: Task[?], c: Channel[?]) extends Event
    case class WakeReader(a: Task[?], b: Task[?]) extends Event
    case class Resuming(t: Task[?], queue: Seq[Task[?]]) extends Event
  }

  trait Scheduler {
    def currentTask: Task[?]
    def launchTask[T](r: => T): Task[T]
    def suspendCurrentTask(): Unit
    def scheduleTask(task: Task[?]): Unit
    def cancelTask(task: Task[?]): Unit
    def runBlocking[T](block: => T): T
    def addLog(e: Event): Unit
  }

  object Scheduler {
    import scala.util.DynamicVariable


    def apply(): Scheduler = new ContinuationScheduler()

    private val scheduler = new DynamicVariable[Scheduler](null)

    private[scheduler] def current: Scheduler = {
      val s = scheduler.value
      if (s == null) {
        throw new IllegalStateException("No scheduler is currently set")
      }
      s
    }

    def launchTask[T](r: => T): Task[T] = current.launchTask(r)
    def scheduleTask(task: Task[?]): Unit = current.scheduleTask(task)
    def suspendCurrentTask(): Unit = current.suspendCurrentTask()
    def cancelTask(task: Task[?]): Unit = current.cancelTask(task)

    def blocking[T](s: Scheduler)(block: => T): T = scheduler.withValue(s)(current.runBlocking(block))
    
    def addLog(e: Event): Unit = current.addLog(e)

  }

}



