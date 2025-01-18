package framework

import gears.async.Async
import gears.async.default.given

import Result.*

class Transaction {

}


abstract class Sequence[T](using Sim, Async) {

  var channel = Channel[Option[T]]()

  def body: Unit

  def doYield(t: T): Unit = {
    channel.send(Some(t))
  }

  def next(using Async): Option[T] = {
    channel.read() match {
      case Ok(t) => t
      case Err(_) => None
    }
  }

  def foreach(f: T => Unit)(using Async): Unit = {
    while(true) {
      next match {
        case Some(t) => f(t)
        case None => return
      }
    }
  }

  def start(): Unit = {
    body
    channel.send(None)
  }

}

abstract class FeedbackSequence[A,B](using Sim, Async) {

  val channel = Channel[Option[A]]()
  val respChannel = Channel[B]()

  def body: Unit

  def doYield(t: A): B = {
    channel.send(Some(t))
    respChannel.read() match {
      case Ok(b) => b
      case Err(_) => throw new Exception("No response")
    }
  }

  def next(using Async): Option[A] = {
    channel.read() match {
      case Ok(t) => t
      case Err(_) => None
    }
  }

  def respond(b: B): Unit = {
    respChannel.send(b)
  }

  def run(): Unit = {
    body
    channel.send(None)
  }

}



