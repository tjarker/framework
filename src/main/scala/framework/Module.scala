package framework

import scala.collection.mutable

import framework.*
import framework.types._
import framework.Module.ModuleBuilderContext
import framework.ClockDomain
import framework.Time.*

object Module {
  class ModuleBuilderContext
}

trait Module(val libPath: String) {

  val name: String

  val domains = mutable.ArrayBuffer[ClockDomain]()

  lazy val ports = domains.flatMap(_.ports)

  def inputs = ports.collect { case i: Input[_] => i }
  def outputs = ports.collect { case o: Output[_] => o }

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

  given ModuleBuilderContext = new ModuleBuilderContext


  def time: SimulationTime = Simulation.time

}
