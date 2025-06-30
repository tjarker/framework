package framework.types

import _root_.framework.simulation.Sim

object Extensions {

  extension (w: Int) {
    def W: Width = Width(w)
  }

  extension [T <: Data](p: Port[T]) {
    def peek[V](using PeekHandler[T, V], Sim): V = {
      summon[PeekHandler[T, V]].peek(p)
    }
  }
  extension [T <: Data](p: Input[T]) {
    def peekMonitor[V](using PeekHandler[T, V], Sim): V = {
      summon[PeekHandler[T, V]].peekMonitor(p)
    }
  }

  extension [T <: Data](p: Output[T]) {
    inline def expect[V](value: V)(using PeekHandler[T, V], Sim): Unit = {
      if p.peek != value then
        summon[Sim].logger.error(
          "sim",
          s"Expected ${value.toString()}, got ${p.peek.toString()}"
        )
    }
  }

  extension [T <: Bits, V](p: Input[T]) {
    def poke(value: V)(using PokeHandler[T, V], Sim): Unit = {
      summon[PokeHandler[T, V]].poke(p, value)
    }
  }

  extension (p: Port[Bool]) {
    def stepUntilRising(using Sim): Unit = {
      while (!p.peek[Boolean]) {
        summon[Sim].ctrl.dut.portToClockDomain(p).clock.step()
      }
    }
  }

  extension (p: ClockPort) {
    def step(steps: Int = 1)(using Sim): Unit = {
      summon[Sim].step(p, steps)
    }
    def stepUntil(pred: => Boolean)(using Sim): Int = {
      var cnt = 0
      while (!pred) {
        summon[Sim].step(p, 1)
        cnt += 1
        if cnt > 1000 then
          throw new Exception(
            s"${summon[Sim].hierarchicalTaskName} timeout on clock $p"
          )
      }
      cnt
    }
  }

  extension (p: ResetPort) {
    def assert()(using Sim): Unit = {
      summon[Sim].poke(p, 1)
    }
    def deassert()(using Sim): Unit = {
      summon[Sim].poke(p, 0)
    }
    def peek(using Sim): BigInt = {
      summon[Sim].peek(p)
    }
  }
}
