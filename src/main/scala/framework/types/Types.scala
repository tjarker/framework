package framework.types


import scala.concurrent.ExecutionContext
import framework.simulation.Sim
import Extensions.*


trait TypeContext {
  def register[T <: Bits](b: T): T
}
object NullContext extends TypeContext {
  def register[T <: Bits](b: T): T = b
}

case class Width(width: Int) {
  override def toString(): String = s"${width - 1}:0"
  def toInt: Int = width
}



trait Bits {
  def width: Width
}

trait Data extends Bits


class Clock extends Bits {
  val width = 1.W
}

class Reset extends Bits {
  val width = 1.W
}

