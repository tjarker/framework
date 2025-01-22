package framework

import gears.async.Async
import gears.async.default.given

import scala.collection.mutable

import Result.*

abstract class Transaction {

  def copy(): this.type
}

abstract class Sequence[A <: Transaction,B <: Transaction](using Sim, Async.Spawn) extends Reportable {

  val channel = Channel[Option[A]]()
  val respChannel = Channel[B]()

  protected def body(): Unit

  protected def yieldTx(t: A): B = {
    channel.send(Some(t))
    respChannel.read() match {
      case Ok(b) => b
      case Err(_) => throw new Exception("No response")
    }
  }

  protected def yieldSeq(seq: Sequence[A,B]): Unit = {
    val resp = mutable.ListBuffer[B]()
    seq.foreach(t => yieldTx(t))
  }

  protected def yieldSeq(seq: Seq[A]): Seq[B] = {
    for(t <- seq) yield yieldTx(t)
  }

  def next()(using Sim, Async): Option[A] = {
    channel.read() match {
      case Ok(t) => t
      case Err(_) => None
    }
  }

  def respond(b: B)(using Sim, Async): Unit = {
    respChannel.send(b)
  }

  def foreach(f: A => B)(using Sim, Async): Unit = {
    while(true) {
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
    body()
    channel.send(None)
  }

  def waitUntilDone()(using Async): Unit = {
    runner.join()
  }

}



