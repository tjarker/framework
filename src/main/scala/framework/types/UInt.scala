package framework.types

class UInt(val width: Width) extends Data {
  override def toString(): String = s"UInt($width)"
}

object UInt {
  def apply(width: Width)(using ctx: TypeContext = NullContext): UInt = {
    ctx.register(new UInt(width))
  }
}
