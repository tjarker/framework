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
