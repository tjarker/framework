package framework

import scala.util.DynamicVariable

import gears.async.*
import gears.async.default.given

import types.*
import Time.*
import ModuleInterface.{ClockDomain, Register}

import scala.collection.mutable

import scala.util.Success
import scala.reflect.ClassTag

trait Sim {

  def currentClock: ClockPort

  def withClock(c: ClockPort): Sim

  def hierarchicalThreadName: String

  def addChildThread(f: Future[?]): Unit

  def getChildThreads: List[Future[?]]

  def ctrl: SimulationController

  def logger: Logger = Logger(true)

  def registerCurrentThread()(using Async): Unit
  def deregisterCurrentThread()(using Async): Unit

  def poke(p: Input[Bits], value: BigInt)(using Async): Unit
  def peek(p: Port[Bits])(using Async): BigInt

  def peekReg(r: Register)(using Async): BigInt

  def step(c: ClockPort, steps: Int)(using Async): Unit

  def step(steps: Int)(using Async): Unit = step(currentClock, steps)

  def join(t: Thread)(using Async): Unit

  def finish()(using Async): Unit

  def abort(e: Throwable)(using Async): Unit

  def time: SimulationTime

}

case class ForkContext(c: Option[Component])

def withClock[T](c: ClockPort)(block: (Sim, Async) ?=> T)(using Sim, Async) = {
    val s = summon[Sim]
    
    val newS = s.withClock(c)

    block(using newS, summon[Async])
}

def stepDomain(steps: Int)(using Sim, Async) = {
    summon[Sim].step(steps)
}

class Fork[T](name: String, block: (Sim, Async.Spawn) ?=> T, group: Seq[Fork[?]])(using Sim, Async.Spawn) {

    val s = summon[Sim]
    val a = summon[Async.Spawn]

    val sim = new Simulation(s.ctrl, SyncChannel(), name, s.currentClock)

    var vThread = Option.empty[Thread]
    
    val future = Future {
      vThread = Some(Thread.currentThread)
      sim.registerCurrentThread()
      val r = try {
        block(using sim, a)
      } catch {
        case e: java.util.concurrent.CancellationException => 
          //sim.logger.info("sim", s"Thread $name cancelled")
        case e: Throwable =>
          sim.logger.error("sim", s"Thread $name failed: $e")
          sim.abort(e)
      }
      sim.deregisterCurrentThread()
      r
    }

    s.addChildThread(future)

    def join(): Unit = {
      s.join(vThread.get)
      group.foreach(_.join())
    }

    def fork[T](block: (Sim, Async.Spawn) ?=> T)(using Sim, Async.Spawn): Fork[T] = {
      val s = summon[Sim]
      val name = s.hierarchicalThreadName + "." + s.getChildThreads.size
      Fork(name, block, Seq(this) ++ group)
    }

  }

object Simulation {

  def apply[M <: ModuleInterface](m: M, timeUnit: Time, wave: Option[String] = None, debug: Boolean = false)(
      block: (Sim, Async.Spawn) ?=> M => Unit
  ): Unit = Async.blocking {
    val ctrl = new SimulationController(SyncChannel(), m, timeUnit, debug, wave)
    val sim = new Simulation(ctrl, SyncChannel(), "root", m.domains.head.clock)
    given Sim = sim
    given ForkContext = ForkContext(None)
    val controller = Future(ctrl.run())
    Future {
      sim.registerCurrentThread()
      block(m)
      sim.finish()
    }
    controller.awaitResult
    Logger.success(s"Simulation of ${m.name} finished")
  }

  

  def fork[T](block: (Sim, Async.Spawn) ?=> T)(using Sim, Async.Spawn): Fork[T] = {
    val s = summon[Sim]
    val name = s.hierarchicalThreadName + "." + s.getChildThreads.size
    Fork(name, block, Seq.empty)
  }

  def forkComp[T](c: Component, phase: String, block: (Sim, Async.Spawn) ?=> T)(using Sim, Async.Spawn): Fork[T] = {
    val s = summon[Sim]
    val name = s.hierarchicalThreadName + "." + s.getChildThreads.size + s"(${c.name} in $phase)"
    Fork(name, block, Seq.empty)
  }

}

class Simulation(
    val ctrl: SimulationController,
    response: SyncChannel[SimulationController.Response],
    val hierarchicalThreadName: String,
    val currentClock: ClockPort
) extends Sim {

  import SimulationController.Command.*
  import SimulationController.Response.*

  private val childThreads = collection.mutable.ListBuffer[Future[?]]()

  def withClock(c: ClockPort): Sim = {
    new Simulation(ctrl, response, hierarchicalThreadName, c)
  }

  def addChildThread(f: Future[?]): Unit = {
    childThreads += f
  }

  def getChildThreads: List[Future[?]] = childThreads.toList

  def registerCurrentThread()(using Async): Unit = {
    ctrl.sendCommand(RegisterThread(Thread.currentThread, hierarchicalThreadName, response))
  }

  def deregisterCurrentThread()(using Async): Unit = {
    ctrl.sendCommand(DeregisterThread(Thread.currentThread))
  }

  def poke(p: Input[Bits], value: BigInt)(using Async): Unit = {
    ctrl.sendCommand(Poke(Thread.currentThread, p, value))
  }

  def peek(p: Port[Bits])(using Async): BigInt = {
    ctrl.sendCommand(Peek(Thread.currentThread, p))
    val r = response.read() match {
      case Right(Peeked(value)) => value
      case _ => throw new RuntimeException("Unexpected response")
    }
    r
  }

  def peekReg(r: Register)(using Async): BigInt = {
    ctrl.sendCommand(PeekReg(Thread.currentThread, r))
    val res = response.read() match {
      case Right(Peeked(value)) => value
      case _ => throw new RuntimeException("Unexpected response")
    }
    res
  }

  def step(c: ClockPort, steps: Int)(using Async): Unit = {
    ctrl.sendCommand(Step(Thread.currentThread, c, steps))
    response.read() match {
      case Right(Stepped) => return
      case _              => throw new RuntimeException("Unexpected response")
    }
  }

  def join(t: Thread)(using Async): Unit = {
    ctrl.sendCommand(WaitForThread(Thread.currentThread, t))
    response.read() match {
      case Right(Joined) => return
      case _              => throw new RuntimeException("Unexpected response")
    }
  }

  def finish()(using Async): Unit = {
    ctrl.sendCommand(Finish(Thread.currentThread))
  }

  def abort(e: Throwable)(using Async): Unit = {
    ctrl.sendCommand(Abort(Thread.currentThread, e))
  }

  def time: SimulationTime = ctrl.time

}

object SimulationController {

  enum Command(origin: Thread) {
    case RegisterThread(t: Thread, name: String, response: SyncChannel[Response])
        extends Command(t)
    case DeregisterThread(t: Thread) extends Command(t)

    case Poke(t: Thread, p: Input[Bits], value: BigInt) extends Command(t)
    case Peek(t: Thread, p: Port[Bits]) extends Command(t)
    case Step(t: Thread, c: ClockPort, steps: Int) extends Command(t)

    case PeekReg(t: Thread, r: Register) extends Command(t)

    case SendToChannel[T](t: Thread, ch: framework.Channel[T]) extends Command(t)
    case WaitForChannel[T](t: Thread, ch: framework.Channel[T]) extends Command(t)

    case WaitForThread(t: Thread, toBeJoined: Thread) extends Command(t)

    case Finish(t: Thread) extends Command(t)

    case Abort(t: Thread, e: Throwable) extends Command(t)
  }

  enum Response {
    case Peeked(value: BigInt)
    case Stepped
    case Joined
  }

  enum ThreadStatus {
    case Running
    case WaitForStep
    case BlockedSend
    case BlockedRead
    case SelfBlocked
    case JoinBlocked(t: String)
  }

}

class SimulationController(
    commands: SyncChannel[SimulationController.Command],
    val dut: ModuleInterface,
    timeUnit: Time,
    debug: Boolean,
    wave: Option[String]
) {

  import SimulationController.*
  import Command.*
  import Response.*


  import sys.process.*

  import java.nio.file.{Files, Paths, StandardOpenOption}
  import java.nio.charset.StandardCharsets
  import java.nio.file.Path

  import com.sun.jna.*

  val p = Paths.get(s"test/${dut.name}")
  HarnessGenerator.generate(dut, p)
  MakefileGenerator.generate(dut, p)

  Process("make clean_copies all", p.toFile).!!

  val libPath = s"${p.toAbsolutePath}/build/lib${dut.name}.so"
  val libCopy = s"${p.toAbsolutePath}/build/lib${dut.name}_${java.time.Instant.now().toEpochMilli}.so"
  Files.copy(Paths.get(libPath), Paths.get(libCopy))

  

  val opts = new java.util.HashMap[String, Int]()
  opts.put(Library.OPTION_OPEN_FLAGS, 2)
  val so = NativeLibrary.getInstance(libCopy, opts)

  val model =
    VerilatorInterface(
      so,
      dut,
      wave.getOrElse(null),
      timeUnit
    )

  val time = SimulationTime(null)

  private val respond =
    collection.mutable.Map[Thread, SendableChannel[Response]]()
  private val threadStatus = collection.mutable.Map[Thread, ThreadStatus]()
  private val names = collection.mutable.Map[Thread, String]()

  private val sends = collection.mutable.Map[framework.Channel[?], Thread]()
  private val reads = collection.mutable.Map[framework.Channel[?], Thread]()
  private val joins = collection.mutable.Map[Thread, collection.mutable.ListBuffer[Thread]]()
  private val queue = InteractionQueue()

  private var finish = false
  private var abort = Option.empty[(Thread, Throwable)]

  val logger = Logger(debug)

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

  logger.info("sim", dut.portToId.toSeq.sortBy(_._2).mkString("\n"))
  logger.info("sim", portState.toString)
  logger.info("sim", uncommitedPortState.toString)
  logger.info("sim", inputDriveSkew.toString)

  def sendCommand(c: Command)(using Async): Unit = {
    commands.send(c)
  }

  def run()(using Async): Unit = {
    logger.info("ctrl", "Waiting for command")
    commands.read() match {
      case Left(_) => logger.error("ctrl", "Unexpected command")
      case Right(c) =>
        //logger.info("ctrl", s"Handling command $c")
        handleCommand(c)
    }
    while (true) {

      if (finish) {
        logger.info("ctrl", "Finishing")
        return
      }

      if (abort.isDefined) {
        val (t, e) = abort.get
        logger.error("ctrl", s"Aborting due to thread ${names(t)}: ${e}")
        return
      }

      if (threadStatus.isEmpty) {
        logger.info("ctrl", "All threads deregistered, exiting")
        return
      }

      logger.info("ctrl", s"""Threads:
                    |  - ${threadStatus.map((t, s) => s"${names(t)}($s) [$t]").mkString("\n  - ")} """.stripMargin)

      
      if (threadStatus.forall(_._2 != ThreadStatus.Running)) {
        logger.info("ctrl", "All threads sleeping")

        val nextTime = queue.nextInteractionTime

        logger.info("ctrl", s"Next interaction at time: $nextTime")

        if (nextTime == time) {
          logger.info("ctrl", "Already at correct time")
        } else if (nextTime > time) {
          logger.info("ctrl", s"Advancing time to $nextTime")
          time.set(nextTime)
          model.tick(nextTime)
        } else throw new RuntimeException("Time went backwards")

        val interactions = queue.getInteractionsForThisTime

        logger.info(
          "ctrl",
          s"Handling interactions: \n  - ${interactions.mkString("  - ")}"
        )

        interactions.foreach { i =>
          logger.info("ctrl", s"Handling interaction $i")
          handleInteraction(i)
        }

      } else {
        

        logger.info("ctrl", "Waiting for command")
        commands.read() match {
          case Left(_) => logger.error("ctrl", "Unexpected command")
          case Right(c) =>
            logger.info("ctrl", s"Received command $c")
            handleCommand(c)
        }

      }
    }
  }

  private def handleInteraction(i: Interaction)(using Async) = i match
    case Interaction.Drive(t, p, value) =>
      model.pokeInput(dut.portToId(p), value, p.width.toInt)
      uncommitedPortState(p) = false
      logger.info("cmc", s"Driven $p with ${value.toString(16)}")

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
      threadStatus(thread) = ThreadStatus.Running
      respond(thread).send(Stepped)
      logger.info("sim", s"Released ${names(thread)}")

  private def handleCommand(c: Command)(using Async) = c match
    case RegisterThread(t, name, response) =>
      threadStatus(t) = ThreadStatus.Running
      respond(t) = response
      names(t) = name
      logger.info("cmd", s"Registered thread $name")

    case DeregisterThread(t) =>
      logger.info("cmd", s"Deregistered thread ${names(t)}")

      if (joins.keys.toSeq.contains(t)) {
        logger.info("cmd", s"Thread ${names(t)} has threads waiting for it")
        val waitingThreads = joins(t)
        waitingThreads.foreach { wt =>
          logger.info("cmd", s"Waking up thread ${names(wt)}")
          respond(wt).send(Joined)
          threadStatus(wt) = ThreadStatus.Running
        }
        joins.remove(t)
      }
      threadStatus.remove(t)
      respond.remove(t)
      

    case Poke(t, p, value) =>
      if (uncommitedPortState(p))
        logger.warning("cmd", s"Multiple drivers for $p")
      logger.info("cmd", s"Thread ${names(t)} poking $p with $value")
      portState(p) = value
      uncommitedPortState(p) = true
      queue.add(
        Interaction.Drive(
          (nextNegEdge(dut.portToClockDomain(p)) + inputDriveSkew(p)).absolute,
          p,
          value
        )
      )

    case Peek(t, p) =>
      val v = p match
        case Input(_)  => 
          model.peekInput(dut.portToId(p))
        case Output(_) => model.peekOutput(dut.portToId(p), p.width.toInt)
      logger.info("cmd", s"Thread ${names(t)} peeked $p = $v")
      respond(t).send(Peeked(v))

    case PeekReg(t, r) =>
      val v = model.peekRegister(dut.regToId(r), r.w.toInt)
      logger.info("cmd", s"Thread ${names(t)} peeked register $r = $v")
      respond(t).send(Peeked(v))

    case Step(t, c, steps) =>
      val wakeup = time + c.period * steps
      queue.add(Interaction.Release(wakeup.absolute, t))
      threadStatus(t) = ThreadStatus.WaitForStep
      logger.info("cmd", s"Thread ${names(t)} want to step $c by $steps (wake up at $wakeup)")

    case SendToChannel(t, ch) =>
      logger.info("cmd", s"Thread ${names(t)} sent to channel")
      if (reads.contains(ch)) {
        
        val r = reads(ch)
        logger.info("cmd", s"Channel has already ${names(r)} someone waiting")
        reads.remove(ch)
        threadStatus(r) = ThreadStatus.Running
      } else {
        sends(ch) = t
        threadStatus(t) = ThreadStatus.BlockedSend
        logger.info("cmd", s"Channel has no one waiting. Marked thread ${names(t)} as sleeping")
      }
      
    case WaitForChannel(t, ch) =>
      logger.info("cmd", s"Thread ${names(t)} waiting for channel")
      if (sends.contains(ch)) {
        logger.info("cmd", s"Channel has already someone sending")
        val s = sends(ch)
        sends.remove(ch)
        threadStatus(s) = ThreadStatus.Running
      } else {
        reads(ch) = t
        threadStatus(t) = ThreadStatus.BlockedRead
        logger.info("cmd", s"Channel has no one sending. Marked thread ${names(t)} as sleeping")
      }
      
    case WaitForThread(t, toBeJoined) =>
      logger.info("cmd", s"Thread ${names(t)} wants to wait for thread ${names(toBeJoined)}")

      if (threadStatus.keys.toSeq.contains(toBeJoined)) {
        logger.info("cmd", s"Thread ${names(toBeJoined)} still running. Marked thread ${names(t)} as sleeping")
        threadStatus(t) = ThreadStatus.JoinBlocked(names(toBeJoined))
        joins.getOrElseUpdate(toBeJoined, collection.mutable.ListBuffer()).addOne(t)
      } else {
        logger.info("cmd", s"Thread ${names(toBeJoined)} has already stopped. Continuing thread ${names(t)}")
        respond(t).send(Joined)
      }


    case Finish(t) => finish = true
    case Abort(t, e) => abort = Some(t -> e)
}
