package framework

import Result.*

import gears.async.{Async, ChannelMultiplexer}

import scala.collection.mutable

class DriverPort[A <: Transaction, B <: Transaction] {
  val tx = ReceiverPort[A]()
  val resp = SenderPort[B]()

  def connect(s: SequencerPort[A,B]): Unit = {
    tx.connect(s.tx)
    resp.connect(s.resp)
  }
}

class SequencerPort[A <: Transaction, B <: Transaction] {
  val tx = SenderPort[A]()
  val resp = ReceiverPort[B]()

  def connect(d: DriverPort[A,B]): Unit = {
    tx.connect(d.tx)
    resp.connect(d.resp)
  }


  def drive(a: A)(using Sim, Async): B = {
    tx.send(a)
    resp.read() match {
      case Ok(b) => b
      case Err(_) => throw new Exception("No response")
    }
  }
}

abstract class Driver[A <: Transaction, B <: Transaction](using Hierarchy)
    extends Component
    with SimulationPhase {

  val port = DriverPort[A, B]()

  private var drivenCnt = 0

  protected def next()(using Sim, Async): A = {
    info("Waiting for next transaction")
    port.tx.read() match {
      case Ok(t) => {
        this.synchronized { drivenCnt += 1 }
        t
      }
      case Err(_) => throw new Exception("No transaction")
    }
  }

  protected def respond(b: B)(using Sim, Async): Unit = port.resp.send(b)

  def numOfDrivenTxs: Int = this.synchronized { drivenCnt }

}


abstract class Monitor[T <: Transaction](using Hierarchy)
    extends Component
    with SimulationPhase {

  private var observedCnt = 0

  val listeners = mutable.ListBuffer[SenderPort[T]]()

  def publish(t: T)(using Sim, Async): Unit = {

    this.synchronized { observedCnt += 1 }
    listeners.synchronized {
      listeners.foreach(_.send(t))
    }

  }

  def addListener(c: ReceiverPort[T]): Unit = listeners.synchronized {
    val sender = SenderPort[T]()
    sender.connect(c)
    listeners += sender
  }

  def numOfObservedTxs: Int = this.synchronized { observedCnt }

}


abstract class AnalysisComponent[T <: Transaction](using Hierarchy) extends Component, SimulationPhase {

  val port = ReceiverPort[T]()

  protected def next()(using Sim, Async): T = {
    port.read() match {
      case Ok(t) => t
      case Err(_) => throw new Exception("No transaction")
    }
  }

}

abstract class Scoreboard[T <: Transaction](using Hierarchy) extends AnalysisComponent[T] {
  
}

class Sequencer[A <: Transaction, B <: Transaction](using Hierarchy) extends Component
    with SimulationPhase {

  val port = SequencerPort[A, B]()

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
      port.drive(t)
    }
  }

}
