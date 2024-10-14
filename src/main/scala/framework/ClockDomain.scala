package framework

import scala.collection.mutable

import framework.types.*

case class ClockDomain(
    val clock: Input[Clock],
    val reset: Option[Input[Reset]],
    val inputs: mutable.ArrayBuffer[Input[Bits]],
    val outputs: mutable.ArrayBuffer[Output[Bits]]
) {

  def ports: Seq[Port[Bits]] = Seq(clock) ++ reset.toSeq ++ inputs ++ outputs

  def period: Time = clock.period

  def halfPeriod: Time = clock.period / 2

  override def toString(): String = {
    s"ClockDomain(${clock}, ${reset}, ${inputs.mkString("[", ", ", "]")}, ${outputs.mkString("[", ", ", "]")})"
  }
}
