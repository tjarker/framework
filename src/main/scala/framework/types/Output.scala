package framework.types

import framework.Module.ModuleBuilderContext
import framework.Naming

class Output[+T <: Bits](
    val name: String,
    val t: T,
    val ctx: ModuleBuilderContext
) extends Port[T] {
  val width = t.width

  override def toString(): String = s"out $name: $t"
}
object Output {

  import scala.quoted.*

  private def outputWithImplicitName[T <: Bits](
      t: Expr[T],
      ctx: Expr[ModuleBuilderContext]
  )(using Quotes, Type[T]): Expr[Output[T]] = {
    '{ new Output[T](${ Naming.enclosingTermName }, $t, $ctx) }
  }

  inline def apply[T <: Bits](
      t: T
  )(using ctx: ModuleBuilderContext): Output[T] = {
    ${ outputWithImplicitName('t, 'ctx) }
  }

  def apply[T <: Bits](name: String, t: T)(using
      ctx: ModuleBuilderContext
  ): Output[T] = {
    new Output(name, t, ctx)
  }

  def unapply[T <: Bits](o: Output[T]): Option[(String, T)] = Some(o.name, o.t)
}
