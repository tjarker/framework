package framework.types

import Types.*

class Bool() extends Data {
  val width = 1.W
  override def toString(): String = "Bool"
}

object Bool {
  def apply()(using ctx: TypeContext = NullContext): Bool = {
    ctx.register(new Bool())
  }
}
