package framework

import scala.collection.mutable

import types.*
import Time.*
import java.nio.file.Path

object ModuleInterface {

  case class ModuleBuilderContext(ports: mutable.ArrayBuffer[Port[Bits]]) {
    def register[T <: Port[Bits]](port: T): T = {
      ports += port.asInstanceOf[Port[Bits]]
      port
    }
  }

  case class Register(w: Width, path: String)

  case class ClockDomain(
      clock: ClockPort,
      ports: Seq[Port[Bits]]
  ) {

    def period: Time = clock.period

    def halfPeriod: Time = clock.period / 2

    override def toString(): String = {
      s"ClockDomain(${clock}, ${ports.mkString("[", ", ", "]")})"
    }
  }


}

trait ModuleInterface(val files: String*) {

  import ModuleInterface.*

  val ctx = ModuleBuilderContext(mutable.ArrayBuffer())
  given ModuleBuilderContext = ctx

  def name: String = this.getClass.getSimpleName

  val domains = mutable.ArrayBuffer[ClockDomain]()

  lazy val ports = ctx.ports.toSeq

  lazy val inputs = ports.collect { case p: Input[_] => p }

  lazy val outputs = ports.collect { case p: Output[_] => p }

  lazy val portToId = ports.zipWithIndex.toMap

  lazy val idToPort = portToId.map(_.swap)

  lazy val clockToClockDomain = domains.map { cd =>
    cd.clock -> cd
  }.toMap

  lazy val portToClockDomain = domains.flatMap { cd =>
    cd.ports.map { p =>
      p -> cd
    }
  }.toMap

  def time(using s: Sim): SimulationTime = s.time

  def domain(clk: ClockPort)(ports: Port[Bits]*): Unit = {
    val cd = ClockDomain(
      clk,
      ports
    )
    domains += cd
  }

  def domain(clk: ClockPort, reset: ResetPort)(
      ports: Port[Bits]*
  ): Unit = {
    val cd = ClockDomain(
      clk,
      ports :+ reset
    )
    domains += cd
  }

  val regs: mutable.ArrayBuffer[Register] = mutable.ArrayBuffer()

  lazy val regToId = regs.zipWithIndex.toMap

  def Reg(w: Width, path: String): Register = {
    val r = Register(w, path)
    regs += r
    r
  }

}
