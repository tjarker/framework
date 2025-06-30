package framework.coop

import framework.coop.{Task, TaskCancelledException}
import jdk.internal.vm.{Continuation, ContinuationScope}

import scala.collection.mutable
import scala.util.{Failure, Success}

class ContinuationTask[T](con: Continuation) extends TaskImpl[T] {

  def run(): Unit = con.run()
  def isDone: Boolean = con.isDone

}

class ContinuationScheduler extends Scheduler {

  val readyQueue = new mutable.Queue[ContinuationTask[?]]()
  val scope = new ContinuationScope(this.toString)
  var currentTask: ContinuationTask[?] = null

  def suspendCurrentTask(): Unit = {
    Continuation.`yield`(scope)
    if (currentTask.cancelled)
      throw TaskCancelledException(currentTask.toString)
  }

  def scheduleTask(task: Task[?]): Unit = {
    readyQueue.enqueue(task.asInstanceOf[ContinuationTask[?]])
  }

  def cancelTask(task: Task[?]): Unit = {
    val contTask = task.asInstanceOf[ContinuationTask[?]]
    contTask.cancelled = true
    readyQueue.dequeueAll(_ == contTask)
    readyQueue.prepend(contTask)
  }

  def runBlocking[T](block: => T): T = {
    val task = launchTask(block).asInstanceOf[ContinuationTask[T]]
    runScheduler()
    task.result match {
      case Some(Success(value)) => value
      case Some(Failure(e))     => throw e
      case None => throw new IllegalStateException("Task has not completed")
    }
  }

  def launchTask[T](r: => T): Task[T] = {
    var task: ContinuationTask[T] = null

    val continuation =
      new Continuation(
        scope,
        () => {
          try {
            val res = r
            task.result = Some(Success(res))
          } catch {
            case e: TaskCancelledException =>
              task.result = Some(Failure(e))
            case e: Throwable =>
              task.result = Some(Failure(e))
              task.printException(e)
          } finally {
            task.onComplete()
          }
        }
      )

    task = new ContinuationTask[T](continuation)
    readyQueue.enqueue(task)
    task
  }

  def runScheduler(): Unit = {
    while (readyQueue.nonEmpty) {
      val task = readyQueue.dequeue()
      currentTask = task
      task.run()
    }
  }
}
