package framework

import Result.*

import gears.async.{Async, ChannelMultiplexer}

import scala.collection.mutable

abstract class Driver[A <: Transaction, B <: Transaction](using Hierarchy)
    extends Component
    with SimulationPhase {

  val txChan = Channel[A]()
  val respChan = Channel[B]()

  private var drivenCnt = 0

  protected def next()(using Sim, Async): A = {
    info("Waiting for next transaction")
    txChan.read() match {
      case Ok(t) => {
        this.synchronized { drivenCnt += 1 }
        t
      }
      case Err(_) => throw new Exception("No transaction")
    }
  }

  protected def respond(b: B)(using Sim, Async): Unit = respChan.send(b)

  def numOfDrivenTxs: Int = this.synchronized { drivenCnt }

  def drive(t: A)(using Sim, Async): B = {
    info(s"Driving transaction: $t")
    txChan.send(t)
    info(s"Waiting for response")
    respChan.read() match {
      case Ok(b)  => b
      case Err(_) => throw new Exception("No response")
    }
  }

}

trait Listener[T <: Transaction] {
  def send(t: T)(using Sim, Async): Unit
}

abstract class Monitor[T <: Transaction](using Hierarchy)
    extends Component
    with SimulationPhase {

  private var observedCnt = 0

  val listeners = mutable.ListBuffer[Listener[T]]()

  def publish(t: T)(using Sim, Async): Unit = {

    this.synchronized { observedCnt += 1 }
    listeners.synchronized {
      listeners.foreach(_.send(t))
    }

  }

  def addListener(c: Listener[T]): Unit = listeners.synchronized {
    listeners += c
  }

  def numOfObservedTxs: Int = this.synchronized { observedCnt }

}


abstract class AnalysisComponent[T <: Transaction](using Hierarchy) extends Component, SimulationPhase, Listener[T] {

  val txChan = Channel[T]()

  def send(t: T)(using Sim, Async): Unit = txChan.send(t)

  protected def next()(using Sim, Async): T = {
    txChan.read() match {
      case Ok(t) => t
      case Err(_) => throw new Exception("No transaction")
    }
  }

}

class Sequencer[A <: Transaction, B <: Transaction](
    drv: Driver[A,B]
)(using Hierarchy) extends Component
    with SimulationPhase {

  val seqChan = Channel[Sequence[A, B]]()

  def play(s: Sequence[A, B])(using Sim, Async): Unit = seqChan.send(s)

  def sim()(using Sim, Async.Spawn): Unit = {

    while (true) {
      seqChan.read() match {
        case Ok(s)  => playSeq(s)
        case Err(_) => throw new Exception("No sequence")
      }
    }

  }

  def playSeq(s: Sequence[A, B])(using Sim, Async.Spawn): Unit = {

    s.foreach { t =>
      drv.drive(t)
    }
  }

}
