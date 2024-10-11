package framework

import scala.collection.mutable

import framework.Module.ModuleBuilderContext

object Types {
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

  class UInt(val width: Width) extends Data {
    override def toString(): String = s"UInt($width)"
  }

  class Bool() extends Data {
    val width = 1.W
    override def toString(): String = "Bool"
  }

  class SInt(val width: Width) extends Data {
    override def toString(): String = s"SInt($width)"
  }

  class Clock() extends Bits {
    val width = 1.W
    override def toString(): String = "Clock"
  }

  class Reset() extends Bits {
    val width = 1.W
    override def toString(): String = "Reset"
  }

  trait Port[+T <: Bits] {
    def width: Width
    val ctx: ModuleBuilderContext
    val name: String
  }

  class Output[+T <: Bits](val name: String, val t: T)(using val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString(): String = s"out $name: $t"
  }
  object Output {
    def unapply[T <: Bits](o: Output[T]): Option[(String, T)] = Some(o.name, o.t)
  }

  class Input[+T <: Bits](val name: String, val t: T)(using val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString: String = s"in $name: $t"
  }
  object Input {
    def unapply[T <: Bits](i: Input[T]): Option[(String, T)] = Some(i.name, i.t)
  }

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
