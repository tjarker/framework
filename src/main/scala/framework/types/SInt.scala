package framework.types

class SInt(val width: Width) extends Data {
  override def toString(): String = s"SInt($width)"
}

object SInt {
  def apply(width: Width)(using ctx: TypeContext = NullContext): SInt = {
    ctx.register(new SInt(width))
  }
}
