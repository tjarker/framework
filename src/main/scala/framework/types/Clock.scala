package framework.types

import framework.Time

class Clock(val period: Time) extends Bits {
  val width = 1.W
  override def toString(): String = "Clock"
}


object Clock {
  def apply(period: Time)(using ctx: TypeContext = NullContext): Clock = {
    ctx.register(new Clock(period))
  }
}