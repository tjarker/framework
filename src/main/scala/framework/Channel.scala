package framework

import simulation.Sim

// enum Result[T, E] {
//   case Ok(v: T)
//   case Err(e: E)

//   def unwrap: T = this match {
//     case Ok(v) => v
//     case Err(e) => throw new RuntimeException(s"Error: $e")
//   }
// }

// object Channel {
//   trait ChannelError
//   case object ChannelClosed extends ChannelError
// }

// class Channel[T] {

//   val chan = SyncChannel[T]()

//   def send(t: T)(using Sim): Unit = {
//     // summon[Sim].ctrl.sendCommand(simulation.SimulationController.Command.SendToChannel(Thread.currentThread(), this))
//     // chan.send(t)
//     ???
//   }

//   def read()(using Sim): Result[T, Channel.ChannelError] = {
//     // summon[Sim].ctrl.sendCommand(simulation.SimulationController.Command.WaitForChannel(Thread.currentThread(), this))
//     // chan.read() match {
//     //   case Right(t) => Result.Ok(t)
//     //   case Left(_) => Result.Err(Channel.ChannelClosed)
//     // }
//     ???
//   }

  
// }

import scala.collection.mutable
import _root_.framework.coop.Task

object Channel {
  def apply[T](): Channel[T] = new Channel[T]()

  enum Result[+T] {
    case Ok(value: T)
    case Closed
  }
}

class Channel[T] {
  
  private val valueQueue = new mutable.Queue[T]()
  private val waitingReaders = new mutable.Queue[Task[?]]()
  private var closed = false

  def send(value: T): Unit = {
    valueQueue.enqueue(value)
    if (waitingReaders.nonEmpty) {
      val reader = waitingReaders.dequeue()
      reader.schedule()
    }
  }

  def close(): Unit = {
    closed = true
    waitingReaders.foreach(_.schedule())
  }

  def read(): Channel.Result[T] = {
    if (valueQueue.isEmpty) {
      waitingReaders.enqueue(Task.current)
      Task.suspendCurrent()
    }
    Predef.assert(!valueQueue.isEmpty, "Value queue is empty after waking up")
    if (closed && valueQueue.isEmpty) {
      return Channel.Result.Closed
    } else Channel.Result.Ok(valueQueue.dequeue())
  }
}


class ReceiverPort[T] {
  val chan = Channel[T]()
  def read()(using Sim): Channel.Result[T] = chan.read()
  def connect(sender: SenderPort[T]): Unit = sender.chan match {
    case Some(_) => throw new Exception("Sender port already connected to receiver")
    case None => sender.chan = Some(chan)
  }
}

class SenderPort[T] {
  var chan: Option[Channel[T]] = None
  def send(t: T)(using Sim): Unit = chan.getOrElse(throw new Exception("Sender port not connected to receiver")).send(t)
  def connect(receiver: ReceiverPort[T]): Unit = chan match {
    case Some(_) => throw new Exception("Sender port already connected to receiver")
    case None => chan = Some(receiver.chan)
  }
}