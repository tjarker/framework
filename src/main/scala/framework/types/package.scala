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
    def peek[V](using PeekHandler[T, V], Sim, Async): V = {
      summon[PeekHandler[T,V]].peek(p)
    }
  }

  extension [T <: Data](p: Output[T]) {
    inline def expect[V](value: V)(using PeekHandler[T,V], Sim, Async): Unit = {
      if p.peek != value then summon[Sim].logger.error("sim", s"Expected ${value.toString()}, got ${p.peek.toString()}")
    }
  }

  extension [T <: Bits, V](p: Input[T]) {
    def poke(value: V)(using PokeHandler[T, V], Sim, Async): Unit = {
      summon[PokeHandler[T, V]].poke(p, value)
    }
  }

  extension (p: ClockPort) {
    def step(steps: Int = 1)(using Sim, Async): Unit = {
      summon[Sim].step(p, steps)
    }
    def stepUntil(pred: => Boolean)(using Sim, Async): Unit = {
      while (!pred) {
        summon[Sim].step(p, 1)
      }
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
