package framework

import scala.collection.mutable

import framework.*
import framework.Types.*
import framework.Module.ModuleBuilderContext
import framework.ClockDomain
import framework.Time.*

object Module {
  class ModuleBuilderContext(
      val peek: () => (Port[Bits] => BigInt),
      val poke: () => ((Input[Bits], BigInt) => Unit),
      val step: () => ((Input[Clock], Int) => Unit)
  )
}

trait Module(val libPath: String) {

  val name: String

  val domains = mutable.ArrayBuffer[ClockDomain]()

  lazy val ports = domains.flatMap(_.ports)

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

  var ctrl: SimController = null
  given ModuleBuilderContext = ModuleBuilderContext(
    () => ctrl.peek,
    () => ctrl.poke,
    () => ctrl.step
  )

  var time: SimulationTime = SimulationTime(() => ctrl.tick)

  def destroy(): Unit = {
    ctrl.sim.destroy()
  }

}
