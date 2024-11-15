package framework

import scala.util.DynamicVariable

import gears.async.*
import gears.async.default.given

import framework.* 
import types.*
import Time.*
import java.lang.ModuleLayer.Controller
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

  def apply[M <: Module](m: M, timeUnit: Time)(block: (Sim, Async.Spawn) ?=> M => Unit): Unit = Async.blocking {
    val ctrl = new SimulationController(SyncChannel())
    val sim = new Simulation(ctrl, SyncChannel())
    Future(ctrl.run())
    block(using sim)(m)
  }

  def fork[T](name: String)(block: (Sim, Async.Spawn) ?=> T)(using Sim, Async.Spawn): Future[T] = {
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







class Simulation(val ctrl: SimulationController, response: SyncChannel[SimulationController.Response]) extends Sim {

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
      case _ => throw new RuntimeException("Unexpected response")
    }
  }

  def time: SimulationTime = null

}

object SimulationController {


  enum Command(origin: Thread) {
    case RegisterThread(t: Thread, response: SyncChannel[Response]) extends Command(t)
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

class SimulationController(commands: SyncChannel[SimulationController.Command]) {

  import SimulationController.*
  import Command.*
  import Response.*

  private val respond = collection.mutable.Map[Thread, SendableChannel[Response]]()
  private val threadRunning = collection.mutable.Map[Thread, Boolean]()


  val logger = Logger(true)

  def sendCommand(c: Command)(using Async): Unit = {
    commands.send(c)
  }

  def run()(using Async): Unit = {
    while (true) {
      commands.read() match {
        case Left(_) => 
        case Right(c) => handleCommand(c)
      }
    }
  }


  private def handleCommand(c: Command)(using Async) = c match
    case RegisterThread(t, response) =>
      threadRunning(t) = true
      respond(t) = response
      logger.warning("sim", s"Registered thread $t")
    case DeregisterThread(t) =>
      threadRunning.remove(t)
      respond.remove(t)
      logger.warning("sim", s"Deregistered thread $t")
    case Poke(t, p, value) =>
      logger.warning("sim", s"Poked $p with $value")
    case Peek(t, p) =>
      logger.warning("sim", s"Peeked $p")
      respond(t).send(Peeked(0))
    case Step(t, c, steps) =>
      logger.warning("sim", s"Stepped $c by $steps")
      respond(t).send(Stepped)
  


}