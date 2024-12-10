package framework.types

import framework.macros.Naming
import framework.Time
import framework.ModuleInterface.ModuleBuilderContext

trait Port[+T <: Bits] {
  def width: Width
  def name: String
}

class Input[+T <: Bits](
    val name: String,
    val t: T
) extends Port[T] {
  val width = t.width

  override def toString: String = s"in $name: $t"
}

object Input {

  inline def apply[T <: Bits](t: T)(using ctx: ModuleBuilderContext): Input[T] = {
    ctx.register(new Input(Naming.enclosingTermName, t))
  }

  def apply[T <: Bits](name: String, t: T)(using ctx: ModuleBuilderContext): Input[T] = {
    ctx.register(new Input(name, t))
  }

  def unapply[T <: Bits](i: Input[T]): Option[(String, T)] = Some(i.name, i.t)
}

class Output[+T <: Bits](
    val name: String,
    val t: T
) extends Port[T] {
  val width = t.width

  override def toString(): String = s"out $name: $t"
}

object Output {

  inline def apply[T <: Bits](t: T)(using ctx: ModuleBuilderContext): Output[T] = {
    ctx.register(new Output(Naming.enclosingTermName, t))
  }

  def apply[T <: Bits](name: String, t: T)(using ctx: ModuleBuilderContext): Output[T] = {
    ctx.register(new Output(name, t))
  }

  def unapply[T <: Bits](o: Output[T]): Option[(String, T)] = Some(o.name, o.t)
}


class ClockPort(
    name: String,
    val period: Time
) extends Input[Clock](name, Clock()) {

  override def toString(): String = s"clock $name: $period"
}

object ClockPort {

  inline def apply(period: Time)(using ctx: ModuleBuilderContext): ClockPort = {
    ctx.register(new ClockPort(Naming.enclosingTermName, period))
  }

  def apply(name: String, period: Time)(using ctx: ModuleBuilderContext): ClockPort = {
    ctx.register(new ClockPort(name, period))
  }

  def unapply(cp: ClockPort): Option[(String, Time)] = Some(cp.name, cp.period)
}

class ResetPort(
    name: String
) extends Input[Reset](name, Reset()) {

  override def toString(): String = s"reset $name"
}

object ResetPort {

  inline def apply()(using ctx: ModuleBuilderContext): ResetPort = {
    ctx.register(new ResetPort(Naming.enclosingTermName))
  }

  def apply(name: String)(using ctx: ModuleBuilderContext): ResetPort = {
    ctx.register(new ResetPort(name))
  }

  def unapply(rp: ResetPort): Option[String] = Some(rp.name)
}