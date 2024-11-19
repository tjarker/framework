package framework

import scala.util.DynamicVariable

import gears.async.*
import gears.async.default.given

import types.*
import Time.*
import Module.ClockDomain

import scala.collection.mutable

import scala.util.Success

trait Sim {

  def ctrl: SimulationController

  def registerCurrentThread()(using Async): Unit
  def deregisterCurrentThread()(using Async): Unit

  def poke(p: Input[Bits], value: BigInt)(using Async): Unit
  def peek(p: Port[Bits])(using Async): BigInt

  def step(c: ClockPort, steps: Int)(using Async): Unit

  def time: SimulationTime
}

object Simulation {

  def apply[M <: Module](m: M, timeUnit: Time)(
      block: (Sim, Async.Spawn) ?=> M => Unit
  ): Unit = Async.blocking {
    val ctrl = new SimulationController(SyncChannel(), m, timeUnit)
    val sim = new Simulation(ctrl, SyncChannel())
    given Sim = sim
    Future(ctrl.run())
    Future {
      sim.registerCurrentThread()
      block(m)
      sim.deregisterCurrentThread()
    }.await
    ctrl.logger.info("sim", "Simulation finished")
  }

  def fork[T](
      name: String
  )(block: (Sim, Async.Spawn) ?=> T)(using Sim, Async.Spawn): Future[T] = {
    val s = summon[Sim]
    Future {
      given Sim = new Simulation(s.ctrl, SyncChannel())
      s.registerCurrentThread()
      val r = block
      s.deregisterCurrentThread()
      r
    }
  }

}

class Simulation(
    val ctrl: SimulationController,
    response: SyncChannel[SimulationController.Response]
) extends Sim {

  import SimulationController.Command.*
  import SimulationController.Response.*

  def registerCurrentThread()(using Async): Unit = {
    ctrl.sendCommand(RegisterThread(Thread.currentThread, response))
  }

  def deregisterCurrentThread()(using Async): Unit = {
    ctrl.sendCommand(DeregisterThread(Thread.currentThread))
  }

  def poke(p: Input[Bits], value: BigInt)(using Async): Unit = {
    ctrl.sendCommand(Poke(Thread.currentThread, p, value))
  }

  def peek(p: Port[Bits])(using Async): BigInt = {
    ctrl.sendCommand(Peek(Thread.currentThread, p))
    response.read() match {
      case Right(Peeked(value)) => value
      case _ => throw new RuntimeException("Unexpected response")
    }
  }

  def step(c: ClockPort, steps: Int)(using Async): Unit = {
    ctrl.sendCommand(Step(Thread.currentThread, c, steps))
    response.read() match {
      case Right(Stepped) => return
      case _              => throw new RuntimeException("Unexpected response")
    }
  }

  def time: SimulationTime = ctrl.time

}

object SimulationController {

  enum Command(origin: Thread) {
    case RegisterThread(t: Thread, response: SyncChannel[Response])
        extends Command(t)
    case DeregisterThread(t: Thread) extends Command(t)

    case Poke(t: Thread, p: Input[Bits], value: BigInt) extends Command(t)
    case Peek(t: Thread, p: Port[Bits]) extends Command(t)
    case Step(t: Thread, c: ClockPort, steps: Int) extends Command(t)
  }

  enum Response {
    case Peeked(value: BigInt)
    case Stepped
  }

}

class SimulationController(
    commands: SyncChannel[SimulationController.Command],
    dut: Module,
    timeUnit: Time
) {

  import SimulationController.*
  import Command.*
  import Response.*

  val model =
    VerilatorInterface(
      dut.libPath,
      dut.name,
      "wave/" + dut.name + ".vcd",
      timeUnit
    )

  val time = SimulationTime(null)

  private val respond =
    collection.mutable.Map[Thread, SendableChannel[Response]]()
  private val threadRunning = collection.mutable.Map[Thread, Boolean]()

  private val queue = InteractionQueue()

  val logger = Logger(true)

  val nextNegEdge = mutable.Map[ClockDomain, AbsoluteTime]()
  dut.domains.foreach { cd =>
    nextNegEdge(cd) = 0.fs.absolute
    queue.add(Interaction.PosEdge((cd.period / 2).absolute, cd.clock))
  }

  val portState = mutable.Map[Port[Bits], BigInt]()
  val uncommitedPortState = mutable.Map[Port[Bits], Boolean]()
  dut.ports.foreach { p =>
    portState(p) = 0
    uncommitedPortState(p) = false
  }

  val inputDriveSkew = dut.inputs.map { p =>
    p -> 0.fs
  }.toMap

  logger.info("sim", dut.portToId.mkString("\n"))
  logger.info("sim", portState.toString)
  logger.info("sim", uncommitedPortState.toString)
  logger.info("sim", inputDriveSkew.toString)

  def sendCommand(c: Command)(using Async): Unit = {
    commands.send(c)
  }

  def run()(using Async): Unit = {
    logger.info("sim", "Waiting for command")
    commands.read() match {
      case Left(_) => throw new RuntimeException("Unexpected command")
      case Right(c) =>
        logger.info("sim", s"Handling command $c")
        handleCommand(c)
    }
    while (true) {

      logger.info("sim", "checking whether no more threads exist.")
      if (threadRunning.isEmpty) {
        logger.info("sim", "All threads deregistered, exiting")
        return
      }

      logger.info("sim", "Checking whether all threads are sleeping")
      if (threadRunning.forall(!_._2)) {
        logger.info("sim", "All threads sleeping")

        val nextTime = queue.nextInteractionTime

        logger.info("sim", s"Next interaction at time: $nextTime")

        if (nextTime == time) {
          logger.info("sim", "Already at correct time")
        } else if (nextTime > time) {
          logger.info("sim", s"Advancing time to $nextTime")
          time.set(nextTime)
          model.tick(nextTime)
        } else throw new RuntimeException("Time went backwards")

        val interactions = queue.getInteractionsForThisTime

        logger.info(
          "sim",
          s"Handling interactions: \n  - ${interactions.mkString("  - ")}"
        )

        interactions.foreach { i =>
          logger.info("sim", s"Handling interaction $i")
          handleInteraction(i)
        }

      } else {
        logger.info("sim", "Waiting for command")
        commands.read() match {
          case Left(_) => throw new RuntimeException("Unexpected command")
          case Right(c) =>
            logger.info("sim", s"Handling command $c")
            handleCommand(c)
        }

      }
    }
  }

  private def handleInteraction(i: Interaction)(using Async) = i match
    case Interaction.Drive(t, p, value) =>
      model.pokeInput(dut.portToId(p), value, p.width.toInt)
      uncommitedPortState(p) = false
      logger.info("sim", s"Driven $p with $value")

    case Interaction.PosEdge(t, c) =>
      model.pokeInput(dut.portToId(c), 1, 1)
      val nextEdge = (t + c.period / 2).absolute
      queue.add(Interaction.NegEdge(nextEdge, c))
      nextNegEdge(dut.clockToClockDomain(c)) = nextEdge
      logger.info("sim", s"Posedge $c")

    case Interaction.NegEdge(t, c) =>
      model.pokeInput(dut.portToId(c), 0, 1)
      val nextEdge = (t + c.period / 2).absolute
      queue.add(Interaction.PosEdge(nextEdge, c))
      logger.info("sim", s"Negedge $c")

    case Interaction.Release(t, thread) =>
      threadRunning(thread) = true
      respond(thread).send(Stepped)
      logger.info("sim", s"Released $thread")

  private def handleCommand(c: Command)(using Async) = c match
    case RegisterThread(t, response) =>
      threadRunning(t) = true
      respond(t) = response
      logger.info("sim", s"Registered thread $t")

    case DeregisterThread(t) =>
      threadRunning.remove(t)
      respond.remove(t)
      logger.info("sim", s"Deregistered thread $t")

    case Poke(t, p, value) =>
      if (uncommitedPortState(p))
        logger.warning("sim", s"Multiple drivers for $p")
      logger.info("sim", s"Poking $p with $value")
      portState(p) = value
      uncommitedPortState(p) = true
      logger.info("sim", "adding drive interaction")
      queue.add(
        Interaction.Drive(
          (nextNegEdge(dut.portToClockDomain(p)) + inputDriveSkew(p)).absolute,
          p,
          value
        )
      )
      logger.info("sim", s"Poked $p with $value")

    case Peek(t, p) =>
      val v = p match
        case Input(_)  => model.peekInput(dut.portToId(p))
        case Output(_) => model.peekOutput(dut.portToId(p), p.width.toInt)
      logger.info("sim", s"Peeked $p = $v")
      respond(t).send(Peeked(v))

    case Step(t, c, steps) =>
      queue.add(Interaction.Release((time + c.period * steps).absolute, t))
      threadRunning(t) = false
      logger.info("sim", s"Stepped $c by $steps")

}
