package framework

import gears.async.Async

import scala.collection.mutable


trait Phase

object Phase {

  def run(c: Component)(using Sim, Async.Spawn): Unit = {

    def inner(
        c: Component,
        fs: mutable.ListBuffer[(Component, Fork[?])]
    ): Unit = {
      c match
        case r: SimulationPhase => fs += c -> forkComp(c, "run", { r.sim() })
        case _                  =>
      c.children.foreach(c => inner(c, fs))
    }

    val fs = mutable.ListBuffer[(Component, Fork[?])]()
    inner(c, fs)
    // Logger.info("sim", "Started run phase for components:\n - " + fs.map(_._1.toString()).mkString("\n - "))
  }

  def test(c: Component)(using Sim, Async.Spawn): Unit = {
    def inner(
        c: Component,
        fs: mutable.ListBuffer[(Component, Fork[?])]
    ): Unit = {
      c match
        case r: TestPhase => fs += c -> forkComp(c, "test", { r.test() })
        case _            =>
      c.children.foreach(c => inner(c, fs))
    }
    val fs = mutable.ListBuffer[(Component, Fork[?])]()
    inner(c, fs)
    // Logger.info("sim", "Started test phase for components:\n - " + fs.map(_._1.toString()).mkString("\n - "))
    fs.foreach(_._2.join())
  }

  def report(c: Component): Unit = {
    c match
      case r: ReportPhase => r.report()
      case _              =>
    c.children.foreach(report)
  }

  def reset(c: Component)(using Sim, Async.Spawn): Unit = {
    def inner(
        c: Component,
        fs: mutable.ListBuffer[(Component, Fork[?])]
    ): Unit = {
      c match
        case r: ResetPhase => fs += c -> forkComp(c, "reset", { r.reset() })
        case _             =>
      c.children.foreach(c => inner(c, fs))
    }
    val fs = mutable.ListBuffer[(Component, Fork[?])]()
    inner(c, fs)
    // Logger.info("sim", "Started reset phase for components:\n - " + fs.map(_._1.toString()).mkString("\n - "))
    fs.foreach(_._2.join())
  }

}

trait SimulationPhase extends Phase {
  def sim()(using Sim, Async.Spawn): Unit
}
trait ReportPhase extends Phase {
  def report(): Unit
}

trait ResetPhase extends Phase {
  def reset()(using Sim, Async.Spawn): Unit
}

trait TestPhase extends Phase {
  def test()(using Sim, Async.Spawn): Unit
}
