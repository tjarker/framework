package framework

import scala.collection.mutable

import Types.*

case class ClockDomain(
    val clock: Port[Clock],
    val reset: Option[Port[Reset]],
    val inputs: mutable.ArrayBuffer[Input[Bits]],
    val outputs: mutable.ArrayBuffer[Output[Bits]]
) {
  override def toString(): String = {
    s"ClockDomain(${clock}, ${reset}, ${inputs.mkString("[", ", ", "]")}, ${outputs.mkString("[", ", ", "]")})"
  }
}
