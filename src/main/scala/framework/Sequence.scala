package framework

import gears.async.Async
import gears.async.default.given

import scala.collection.mutable

import Result.*

abstract class Transaction {

}

abstract class Sequence[A <: Transaction, B <: Transaction](using
    Sim,
    Async.Spawn
) extends Reportable {

  val channel = Channel[Option[A]]()
  val respChannel = Channel[B]()

  protected def body(): Unit

  protected def yieldTx(t: A): B = {
    channel.send(Some(t))
    respChannel.read() match {
      case Ok(b)  => b
      case Err(_) => throw new Exception("No response")
    }
  }

  protected def yieldSeq(seq: Sequence[A, B]): Unit = {
    seq.foreach(t => yieldTx(t))
  }

  protected def yieldSeq(seq: Seq[A]): Seq[B] = {
    for (t <- seq) yield yieldTx(t)
  }

  def next()(using Sim, Async): Option[A] = {
    channel.read() match {
      case Ok(t)  => t
      case Err(_) => None
    }
  }

  def respond(b: B)(using Sim, Async): Unit = {
    respChannel.send(b)
  }

  def foreach(f: A => B)(using Sim, Async): Unit = {
    while (true) {
      next() match {
        case Some(t) => {
          respond(f(t))
        }
        case None => return
      }
    }
  }

  override def toString(): String = {
    s"Seq(${super.toString()})"
  }

  private val runner = fork {
    info(s"Starting sequence $this")
    body()
    channel.send(None)
  }

  def waitUntilDone()(using Async): Unit = {
    runner.join()
  }

}

object Sequence {

  class ScalaSeq[A <: Transaction](seq: Seq[A])(using Sim, Async.Spawn)
      extends Sequence[A, Transaction] {

    protected def body(): Unit = {
      for (t <- seq) {
        yieldTx(t)
      }
    }

  }

  class Concat[A <: Transaction, B <: Transaction](seqs: Sequence[A, B]*)(using
      Sim,
      Async.Spawn
  ) extends Sequence[A, B] {

    protected def body(): Unit = {
      for (seq <- seqs) {
        yieldSeq(seq)
      }
    }

  }

  class Map[A <: Transaction, B <: Transaction, C <: Transaction](
      seq: Sequence[A, C],
      f: A => B
  )(using Sim, Async.Spawn)
      extends Sequence[B, C] {

    protected def body(): Unit = {
      seq.foreach { t =>
        yieldTx(f(t))
      }
    }

  }

  class Mix[A <: Transaction, B <: Transaction](
      a: Sequence[A, B],
      b: Sequence[A, B]
  )(using Sim, Async.Spawn)
      extends Sequence[A, B] {

    protected def body(): Unit = {

      var toBeFinished = doStuff()

      yieldSeq(toBeFinished)

    }

    private def doStuff()(using Sim, Async.Spawn): Sequence[A, B] = {
      while (true) {
        if util.Random.nextBoolean() then {
          a.next() match {
            case Some(t) => {
              a.respond(yieldTx(t))
            }
            case None => return b
          }
        } else {
          b.next() match {
            case Some(t) => {
              b.respond(yieldTx(t))
            }
            case None => return a
          }
        }
      }
      return a
    }
  }

}


abstract class Orchestrator(using Sim, Async.Spawn) {


  protected def body(): Unit


  def run(): Unit = fork {
    body()
  }


}