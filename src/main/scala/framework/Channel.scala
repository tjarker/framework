package framework

import gears.async.*

enum Result[T, E] {
  case Ok(v: T)
  case Err(e: E)

  def unwrap: T = this match {
    case Ok(v) => v
    case Err(e) => throw new RuntimeException(s"Error: $e")
  }
}

object Channel {
  trait ChannelError
  case object ChannelClosed extends ChannelError
}

class Channel[T] {

  val chan = SyncChannel[T]()

  def send(t: T)(using Sim, Async): Unit = {
    summon[Sim].ctrl.sendCommand(SimulationController.Command.SendToChannel(Thread.currentThread(), this))
    chan.send(t)
  }

  def read()(using Sim, Async): Result[T, Channel.ChannelError] = {
    summon[Sim].ctrl.sendCommand(SimulationController.Command.WaitForChannel(Thread.currentThread(), this))
    chan.read() match {
      case Right(t) => Result.Ok(t)
      case Left(_) => Result.Err(Channel.ChannelClosed)
    }
  }

  
}

class ReceiverPort[T] {
  val chan = Channel[T]()
  def read()(using Sim, Async): Result[T, Channel.ChannelError] = chan.read()
  def connect(sender: SenderPort[T]): Unit = sender.chan match {
    case Some(_) => throw new Exception("Sender port already connected to receiver")
    case None => sender.chan = Some(chan)
  }
}

class SenderPort[T] {
  var chan: Option[Channel[T]] = None
  def send(t: T)(using Sim, Async): Unit = chan.getOrElse(throw new Exception("Sender port not connected to receiver")).send(t)
  def connect(receiver: ReceiverPort[T]): Unit = chan match {
    case Some(_) => throw new Exception("Sender port already connected to receiver")
    case None => chan = Some(receiver.chan)
  }
}