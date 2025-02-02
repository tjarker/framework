package framework

import gears.async.Async
import gears.async.default.given

import scala.collection.mutable

import scala.reflect.ClassTag

import Result.*


abstract class Sequence[A <: Transaction, B <: Transaction](using
    Hierarchy
) extends Reportable {

  val channel = Channel[Option[A]]()
  val respChannel = Channel[B]()

  protected def body()(using Sim, Async.Spawn): Unit

  protected def yieldTx(t: A)(using Sim, Async): B = {
    channel.send(Some(t))
    respChannel.read() match {
      case Ok(b)  => b
      case Err(_) => throw new Exception("No response")
    }
  }

  protected def yieldSeq(seq: Sequence[A, B])(using Sim, Async): Unit = {
    seq.foreach(t => yieldTx(t))
  }

  protected def yieldSeq(seq: Seq[A])(using Sim, Async): Seq[B] = {
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

  private var runner: Option[Fork[?]] = None

  def start()(using Sim, Async.Spawn): Unit = {
    runner = Some(fork {
      info(s"Starting sequence $this")
      body()
      channel.send(None)
    })
  }

  def waitUntilDone()(using Async): Unit = {
    runner match {
      case Some(r) => r.join()
      case None    => throw new Exception("Sequence not started")
    }
  }

}

object SequenceComposition {

  class ScalaSeq[A <: Transaction](seq: Seq[A])(using Hierarchy)
      extends Sequence[A, Transaction] {

    protected def body()(using Sim, Async.Spawn): Unit = {
      for (t <- seq) {
        yieldTx(t)
      }
    }

  }

  class Concat[A <: Transaction, B <: Transaction](seqs: Sequence[A, B]*)(using
      Hierarchy
  ) extends Sequence[A, B] {

    protected def body()(using Sim, Async.Spawn): Unit = {
      for (seq <- seqs) {
        yieldSeq(seq)
      }
    }

  }

  class Map[A <: Transaction, B <: Transaction, C <: Transaction](
      seq: Sequence[A, C],
      f: A => B
  )(using Hierarchy)
      extends Sequence[B, C] {

    protected def body()(using Sim, Async.Spawn): Unit = {
      seq.foreach { t =>
        yieldTx(f(t))
      }
    }

  }

  class Mix[A <: Transaction, B <: Transaction](
      s: Sequence[A,B]*
  )(using Hierarchy)
      extends Sequence[A, B] {

    protected def body()(using Sim, Async.Spawn): Unit = {

      val list = mutable.ListBuffer(s*)

      while (list.nonEmpty) {
        val idx = scala.util.Random.nextInt(list.size)
        val seq = list(idx)
        seq.next() match {
          case Some(item) => {
            seq.respond(yieldTx(item))
          }
          case None => list.remove(idx)
        }
      }

    }
  }

}
