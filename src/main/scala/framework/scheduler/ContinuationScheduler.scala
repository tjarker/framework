package framework.scheduler

import scala.collection.mutable

import jdk.internal.vm.Continuation
import jdk.internal.vm.ContinuationScope

class ContinuationTask[T](con: Continuation) extends InternalTask[T] {

  def run() = con.run()
  def isDone: Boolean = con.isDone()

}

class ContinuationScheduler extends Scheduler {

  var currentTask: ContinuationTask[?] = null
  val readyQueue = new mutable.Queue[ContinuationTask[?]]()
  val scope = new ContinuationScope(this.toString())

  def launchTask[T](r: => T): Task[T] = {
    var task: ContinuationTask[T] = null

    val continuation =
      new Continuation(
        scope,
        new Runnable {
          def run(): Unit = {
            try {
              val res = r
              task.result = Some(TaskResult.Ok(res))
            } catch {
              case TaskCancelledException =>
                task.result = Some(TaskResult.Canceled)
              case e: Throwable =>
                task.result = Some(TaskResult.Err(e))
                task.printException(e)
            } finally {
              task.onComplete()
            }
          }
        }
      )

    task = new ContinuationTask[T](continuation)
    readyQueue.enqueue(task)
    task
  }

  def suspendCurrentTask(): Unit = {
    Continuation.`yield`(scope)
    if (currentTask.cancelled) throw TaskCancelledException
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

  def runScheduler(): Unit = {
    while (readyQueue.nonEmpty) {
      val task = readyQueue.dequeue()
      currentTask = task
      task.run()
    }
  }

  def runBlocking[T](block: => T): T = {
    val task = launchTask(block).asInstanceOf[ContinuationTask[T]]
    runScheduler()
    task.result match {
      case Some(TaskResult.Ok(value)) => value
      case Some(TaskResult.Err(e))    => throw e
      case Some(TaskResult.Canceled)  => throw TaskCancelledException
      case None                       => throw new IllegalStateException("Task has not completed")
    }
  }

  val log = new mutable.ArrayBuffer[Event]

  def addLog(e: Event): Unit = log += e
}
