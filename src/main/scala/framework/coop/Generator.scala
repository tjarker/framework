package framework.coop

import scala.util.DynamicVariable

/* somehow use continuations directly here. can we support generators directly inside the class body AND possibly in functions? */

object Gen {
  // val ch = new DynamicVariable[Channel[?]](null)

  // def apply[T](block: => Any): Generator[T] = {
  //   new Generator(block)
  // }

  // def emit[T](value: T): Unit = {
  //   val channel = Gen.ch.value.asInstanceOf[Channel[T]]
  //   channel.send(value)
  // }
}

class Generator[T](block: => Any) extends Iterator[T] {

  // val ch = new Channel[T]()

  var done = false

  override def next(): T = ???

  override def hasNext: Boolean = !done

  // launch {
  //   Gen.ch.withValue(ch) {
  //     block
  //   }
  //   done = true
  // }
}
