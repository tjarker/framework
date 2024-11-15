package framework


import gears.async.*

package object types {

  trait TypeContext {
    def register[T <: Bits](b: T): T
  }
  object NullContext extends TypeContext {
    def register[T <: Bits](b: T): T = b
  }

  case class Width(width: Int) {
    override def toString(): String = s"${width - 1}:0"
    def toInt: Int = width
  }

  extension (w: Int) {
    def W: Width = Width(w)
  }

  trait Bits {
    def width: Width
  }

  trait Data extends Bits


  class Clock extends Bits {
    val width = 1.W
  }

  class Reset extends Bits {
    val width = 1.W
  }

  extension [T <: Data](p: Port[T]) {
    def peek(using Sim, Async): BigInt = {
      summon[Sim].peek(p)
    }
  }

  extension [T <: Data](p: Input[T]) {
    def poke(value: BigInt)(using Sim, Async): Unit = {
      summon[Sim].poke(p, value)
    }
  }

  extension (p: ClockPort) {
    def step(steps: Int = 1)(using Sim, Async): Unit = {
      summon[Sim].step(p, steps)
    }
  }

  extension (p: ResetPort) {
    def assert()(using Sim, Async): Unit = {
      summon[Sim].poke(p, 1)
    }
    def deassert()(using Sim, Async): Unit = {
      summon[Sim].poke(p, 0)
    }
    def peek(using Sim, Async): BigInt = {
      summon[Sim].peek(p)
    }
  }
  
}
