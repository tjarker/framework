package framework.scheduler

import framework.scheduler._
import scala.collection.mutable

class Channel[T] {
  private val valueQueue = new mutable.Queue[T]()
  private val waitingReaders = new mutable.Queue[Task[?]]()

  def send(value: T): Unit = {
    // println(s"[${Task.current}] sending $value")
    Scheduler.addLog(Event.Send(Task.current, this, value))
    valueQueue.enqueue(value)
    if (waitingReaders.nonEmpty) {
      val reader = waitingReaders.dequeue()
      Scheduler.addLog(Event.WakeReader(Task.current, reader))
      // println(s"[${Task.current}] waking reader $reader")
      Scheduler.scheduleTask(reader)
    }
  }

  def receive(): T = {
    if (valueQueue.isEmpty) {
      Scheduler.addLog(Event.ReceiveSuspend(Task.current, this))
      waitingReaders.enqueue(Task.current)
      // println(s"[${Task.current}] no value to receive, suspending")
      Scheduler.suspendCurrentTask()
    } else {
      Scheduler.addLog(Event.ReceiveDirect(Task.current, this))
    }
    assert(!valueQueue.isEmpty, "Value queue is empty after waking up")
    // println(s"[${Task.current}] got value")
    valueQueue.dequeue()
  }
}
