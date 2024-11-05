package framework.types

import framework.Module.ModuleBuilderContext
import framework.Naming
import framework.Time
import framework.Time.fs

class Input[+T <: Bits](
    val name: String,
    val t: T,
    val driveSkew: Time,
    val ctx: ModuleBuilderContext
) extends Port[T] {
  val width = t.width

  override def toString: String = s"in $name: $t"
}
object Input {

  inline def apply[T <: Bits](t: T, driveSkew: Time = 0.fs)(using
      ctx: ModuleBuilderContext
  ): Input[T] = {
    new Input(Naming.enclosingTermName, t, driveSkew, ctx)
  }

  def apply[T <: Bits](name: String, t: T, driveSkew: Time)(using
      ctx: ModuleBuilderContext
  ): Input[T] = {
    new Input(name, t, driveSkew, ctx)
  }

  def unapply[T <: Bits](i: Input[T]): Option[(String, T)] = Some(i.name, i.t)
}
