package framework.types

import framework.Util
import framework.Sim

import gears.async.Async

trait PokeHandler[T <: Bits, V] {
  def poke(p: Input[T], value: V)(using Sim, Async): Unit
}

object PokeHandler {
  given PokeHandler[UInt, BigInt] with {
    def poke(p: Input[UInt], value: BigInt)(using Sim, Async): Unit = {
      if (value < 0) throw new RuntimeException(s"Port $p of type UInt cannot be assigned a negative value")
      else if (Util.log2ceil(value) > p.width.toInt) throw new RuntimeException(s"Value $value is too large for port $p")
      summon[Sim].poke(p, value)
    }
  }

  given PokeHandler[UInt, Long] with {
    def poke(p: Input[UInt], value: Long)(using Sim, Async): Unit = {
      p.poke(BigInt(value))
    }
  }

  given PokeHandler[UInt, Int] with {
    def poke(p: Input[UInt], value: Int)(using Sim, Async): Unit = {
      p.poke(BigInt(value))
    }
  }


  given PokeHandler[SInt, BigInt] with {
    def poke(p: Input[SInt], value: BigInt)(using Sim, Async): Unit = {
      if (value.bitLength > p.width.toInt) throw new RuntimeException(s"Value $value is too large for port $p")
      summon[Sim].poke(p, value)
    }
  }

  given PokeHandler[Bool, Boolean] with {
    def poke(p: Input[Bool], value: Boolean)(using Sim, Async): Unit = {
      summon[Sim].poke(p, if value then 1 else 0)
    }
  }
}
