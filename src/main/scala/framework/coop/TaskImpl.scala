package framework.coop

import framework.coop.Task

import scala.collection.mutable
import scala.util.Try

private[coop] trait TaskImpl[T] extends Task[T] {

  val waitingTasks = new mutable.ArrayBuffer[Task[?]](5)
  var result: Option[Try[T]] = None

  var cancelled = false

  def await: Try[T] = {
    if (result.isEmpty) {
      waitingTasks.append(Task.current)
      Scheduler.suspendCurrentTask()
    }
    result.getOrElse(throw new IllegalStateException("Task has not completed"))
  }

  def onComplete(): Unit = {
    waitingTasks.foreach(Scheduler.scheduleTask)
  }

  def printException(e: Throwable): Unit = {
    System.err.println(s"Exception in Task \"$this\" ${e.getMessage}")
    e.getStackTrace.foreach { l =>
      System.err.println(s"\tat $l")
    }
  }

}
