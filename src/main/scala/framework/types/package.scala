package framework

import scala.quoted.Type

package object types {

  trait TypeContext {
    def register[T <: Bits](b: T): T
  }
  object NullContext extends TypeContext {
    def register[T <: Bits](b: T): T = b
  }

  case class Width(width: Int) {
    override def toString(): String = s"${width - 1}:0"
  }

  extension (w: Int) {
    def W: Width = Width(w)
  }

  trait Bits {
    def width: Width
  }

  trait Data extends Bits

  extension [T <: Data](p: Port[T]) {
    def peek: BigInt = {
      p.ctx.peek.apply().apply(p)
    }
  }

  extension [T <: Data](p: Input[T]) {
    def poke(value: BigInt): Unit = {
      p.ctx.poke.apply().apply(p, value)
    }
  }

  extension (p: Input[Clock]) {
    def step(steps: Int = 1): Unit = {
      p.ctx.step.apply().apply(p, steps)
    }
    def period = p.t.period
  }

  extension (p: Input[Reset]) {
    def assert(): Unit = {
      p.ctx.poke.apply().apply(p, 1)
    }
    def deassert(): Unit = {
      p.ctx.poke.apply().apply(p, 0)
    }
    def peek: BigInt = {
      p.ctx.peek.apply().apply(p)
    }
  }
  
}
