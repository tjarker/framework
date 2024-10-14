package framework.types

class Reset() extends Bits {
  val width = 1.W
  override def toString(): String = "Reset"
}

object Reset {
  def apply()(using ctx: TypeContext = NullContext): Reset = {
    ctx.register(new Reset())
  }
}
