package framework.scheduler

import scala.collection.mutable


private[scheduler] trait InternalTask[T] extends Task[T] {

  val waitingTasks = new mutable.ArrayBuffer[Task[?]](5)
  var result: Option[TaskResult[T]] = None

  var cancelled = false

  def await: TaskResult[T] = {
    if (result.isEmpty) {
      waitingTasks.append(Task.current)
      debug(s"Task ${Task.current} is waiting for result of task $this")
      Scheduler.suspendCurrentTask()
    }
    result.getOrElse(throw new IllegalStateException("Task has not completed"))
  }

  def onComplete(): Unit = {
    waitingTasks.foreach(Scheduler.scheduleTask)
  }

  def printException(e: Throwable): Unit = {
    System.err.println(s"Exception in Task \"$this\" ${e.getMessage()}")
    e.getStackTrace().foreach { l =>
      System.err.println(s"\tat $l")
    }
  }

}
