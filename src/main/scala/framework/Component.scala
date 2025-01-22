package framework

import gears.async.Async

import scala.collection.mutable

import scala.reflect.ClassTag
import gears.async.Async
import gears.async.ChannelMultiplexer

import Result.*

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
      c.getChildren.foreach(c => inner(c, fs))
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
      c.getChildren.foreach(c => inner(c, fs))
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
    c.getChildren.foreach(report)
  }

  def reset(c: Component)(using Sim, Async.Spawn): Unit = {
    def inner(
        c: Component,
        fs: mutable.ListBuffer[(Component, Fork[?])]
    ): Unit = {
      c match
        case r: ResetPhase => fs += c -> forkComp(c, "reset", { r.reset() })
        case _             =>
      c.getChildren.foreach(c => inner(c, fs))
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

object Component {
  def root[C <: Component](c: C): C = {
    c.hierarchy = Hierarchy(null, mutable.ListBuffer())
    c.name = "test"
    c.initComponentHierarchy()
    // Logger.info("sim", "done initializing component hierarchy")
    c
  }
}

case class Hierarchy(parent: Component, children: mutable.ListBuffer[Component])

trait Component extends Reportable {

  private[framework] var name: String = ""
  private[framework] var hierarchy: Hierarchy = null
  private[framework] var forkCtx: ForkContext = null

  private[framework] def getChildren = hierarchy.children
  private[framework] def getParent = hierarchy.parent

  private[framework] def initComponentHierarchy()(using
      ct: ClassTag[Component]
  ): Unit = {

    def getAllFields(clazz: Class[?]): Seq[java.lang.reflect.Field] = {
      if (clazz == null || clazz == classOf[Object]) {
        Seq.empty
      } else {
        // Collect fields from the current class
        val fieldsInClass = clazz.getDeclaredFields.toIndexedSeq
        // Collect fields from the superclasses and traits
        val fieldsInSuperclass = getAllFields(clazz.getSuperclass)
        // Collect fields from the traits, which are part of the class hierarchy
        val fieldsInInterfaces = clazz.getInterfaces.flatMap(getAllFields)

        fieldsInClass ++ fieldsInSuperclass ++ fieldsInInterfaces
      }
    }

    getAllFields(this.getClass)
      .filter(field => ct.runtimeClass.isAssignableFrom(field.getType))
      .foreach { field =>
        field.setAccessible(true) // Make private fields accessible
        val c = field.get(this).asInstanceOf[Component]
        c.hierarchy = Hierarchy(this, mutable.ListBuffer())
        c.name = field.getName
        c.forkCtx = ForkContext(Some(c))
        this.hierarchy.children += c
        c.initComponentHierarchy()
      }
  }

  override def toString(): String =
    if (hierarchy.parent != null) s"${hierarchy.parent.toString()}.${name}"
    else name
}

trait ExitConsensus {
  def raiseObjection(): Unit
  def dropObjection(): Unit
  def registerCondition(condition: => Boolean): Unit
}

def runTest[M <: ModuleInterface](dut: M, res: Time)(testConstructor: M => TestCase): Unit = {
  Simulation(dut, res) { dut =>
    val test = Component.root(testConstructor(dut))
    Phase.run(test)
    Phase.reset(test)
    Phase.test(test)
    Phase.report(test)
  }
}

abstract class TestCase extends Component with TestPhase {
}

abstract class Driver[A <: Transaction, B <: Transaction] extends Component with SimulationPhase {

  val txChan = Channel[A]()
  val respChan = Channel[B]()

  def next()(using Sim, Async): A = {
    info("Waiting for next transaction")
    txChan.read() match {
      case Ok(t)  => t
      case Err(_) => throw new Exception("No transaction")
    }
  }

  def respond(b: B)(using Sim, Async): Unit = respChan.send(b)

}

class Sequencer[A <: Transaction, B <: Transaction](txChan: Channel[A], respChan: Channel[B])
    extends Component
    with SimulationPhase {

  val seqChan = Channel[Sequence[A,B]]()

  def play(s: Sequence[A,B])(using Sim, Async): Unit = seqChan.send(s)

  def sim()(using Sim, Async.Spawn): Unit = {

    while (true) {
      info("Waiting for next sequence")
      seqChan.read() match {
        case Ok(s)  => playSeq(s)
        case Err(_) => throw new Exception("No sequence")
      }
    }

  }

  def playSeq(s: Sequence[A,B])(using Sim, Async.Spawn): Unit = {
    info("Playing sequence")

    s.foreach { t =>
      info(s"Playing transaction $t")
      txChan.send(t)
      respChan.read() match {
        case Ok(b) => b
        case Err(_) => throw new Exception("No response")
      }
    }
    info("Sequence done")
  }

}
