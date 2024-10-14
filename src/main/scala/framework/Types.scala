package framework

import scala.collection.mutable

import framework.Module.ModuleBuilderContext
import framework.Time.*

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

  class Clock(val period: Time) extends Bits {
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

  class Output[+T <: Bits](val name: String, val t: T, val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString(): String = s"out $name: $t"
  }
  object Output {

    import scala.quoted.*

    private def outputWithImplicitName[T <: Bits](t: Expr[T], ctx: Expr[ModuleBuilderContext])(using Quotes, Type[T]): Expr[Output[T]] = {
      '{ new Output[T](${Naming.enclosingTermName}, $t, $ctx) }
    }

    inline def apply[T <: Bits](t: T)(using ctx: ModuleBuilderContext): Output[T] = {
      ${ outputWithImplicitName('t, 'ctx) }
    }

    def apply[T <: Bits](name: String, t: T)(using ctx: ModuleBuilderContext): Output[T] = {
      new Output(name, t, ctx)
    }

    def unapply[T <: Bits](o: Output[T]): Option[(String, T)] = Some(o.name, o.t)
  }

  class Input[+T <: Bits](val name: String, val t: T, val driveSkew: Time, val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString: String = s"in $name: $t"
  }
  object Input {

    import scala.quoted.*
    private def inputWithImplicitName[T <: Bits](t: Expr[T], driveSkew: Expr[Time], ctx: Expr[ModuleBuilderContext])(using Quotes, Type[T]): Expr[Input[T]] = {
      '{ new Input[T](${Naming.enclosingTermName}, $t, $driveSkew, $ctx) }
    }

    inline def apply[T <: Bits](t: T, driveSkew: Time = 0.fs)(using ctx: ModuleBuilderContext): Input[T] = {
      ${ inputWithImplicitName('t, 'driveSkew, 'ctx) }
    }

    def apply[T <: Bits](name: String, t: T, driveSkew: Time)(using ctx: ModuleBuilderContext): Input[T] = {
      new Input(name, t, driveSkew, ctx)
    }

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
