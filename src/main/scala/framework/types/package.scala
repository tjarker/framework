package framework


import gears.async.*
import scala.concurrent.ExecutionContext

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
  extension [T <: Data](p: Input[T]) {
    def peekMonitor[V](using PeekHandler[T, V], Sim, Async): V = {
      summon[PeekHandler[T,V]].peekMonitor(p)
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

  extension (p: Port[Bool]) {
        def stepUntilRising(using Sim, Async): Unit = {
            while (!p.peek[Boolean]) {
              summon[Sim].ctrl.dut.portToClockDomain(p).clock.step()
            }
        }
    }

  extension (p: ClockPort) {
    def step(steps: Int = 1)(using Sim, Async): Unit = {
      summon[Sim].step(p, steps)
    }
    def stepUntil(pred: => Boolean)(using Sim, Async): Int = {
      var cnt = 0
      while (!pred) {
        summon[Sim].step(p, 1)
        cnt += 1
        if cnt > 1000 then throw new Exception(s"${summon[Sim].hierarchicalThreadName} timeout on clock $p")
      }
      cnt
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
