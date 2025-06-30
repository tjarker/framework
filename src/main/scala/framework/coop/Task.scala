package framework.coop

import scala.util.Try

/** Represents a cooperative task that can be scheduled and awaited. Tasks can
    * be launched, suspended, and cancelled.
    * @tparam T
    *   the type of the result produced by the task
    */
trait Task[T] {

  /** Awaits the completion of the task and returns the result.
    * @return
    *   a Try containing the result of the task, or an exception if the task
    *   failed
    */
  def await: Try[T]

  /** Cancels the task, preventing it from running if it has not already
    * started.
    */
  def cancel(): Unit = Scheduler.cancelTask(this)

  /** Schedules the task to be run by the scheduler. This method should be
    * called to make the task runnable.
    */
  def schedule(): Unit = Scheduler.scheduleTask(this)
}

/** Exception thrown when a task is cancelled.
  * @param name
  *   the name of the task that was cancelled
  */
case class TaskCancelledException(name: String)
    extends Exception(s"Task $name was cancelled")

object Task {

  /** Returns the current task that is being executed.
    */
  def current: Task[?] = Scheduler.current.currentTask

  /** Suspends the currently running task
    */
  def suspendCurrent(): Unit = Scheduler.suspendCurrentTask()
}
