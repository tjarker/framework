package framework.types

import framework.Util
import framework.simulation.Sim


trait PeekHandler[T <: Data, V] {
  def peek(p: Port[T])(using Sim): V
  def peekMonitor(p: Input[T])(using Sim): V
}


object PeekHandler {
  given PeekHandler[Bool, Boolean] with {
    def peek(p: Port[Bool])(using Sim): Boolean = summon[Sim].peek(p) != 0
    def peekMonitor(p: Input[Bool])(using Sim): Boolean = summon[Sim].peekMonitor(p) != 0
  }

  given PeekHandler[UInt, BigInt] with {
    def peek(p: Port[UInt])(using Sim): BigInt = summon[Sim].peek(p)
    def peekMonitor(p: Input[UInt])(using Sim): BigInt = summon[Sim].peekMonitor(p)
  }
}
