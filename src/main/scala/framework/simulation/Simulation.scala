package framework.simulation

import scala.util.DynamicVariable

import _root_.framework.coop.{Task, Scheduler}
import _root_.framework.types.*
import Time.*
import ModuleInterface.{ClockDomain, Register}
import _root_.framework.Logger

import _root_.framework.Component

import scala.collection.mutable

import scala.util.Success
import scala.reflect.ClassTag

case class ForkContext(c: Option[Component])

def withClockDomain[T](c: ClockPort)(block: Sim ?=> T)(using Sim) = {
  val s = summon[Sim]

  val newS = s.withClock(c)

  block(using newS)
}

def stepClockDomain(steps: Int)(using Sim) = {
  summon[Sim].step(steps)
}

class Fork[T](name: String, block: Sim ?=> T, group: Seq[Fork[?]])(using Sim) {

  val s = summon[Sim]

  val sim = new Simulation(s.ctrl, name, s.currentClock)

  val task = Scheduler.launchTask {
    
    val r =
      try {
        block(using sim)
      } catch {
        case e: java.util.concurrent.CancellationException =>
        // sim.logger.info("sim", s"Thread $name cancelled")
        case e: Throwable =>
          sim.logger.error("sim", s"Thread $name failed: $e")
          sim.abort(e)
      }
    sim.retire(Task.current)
    r
  }

  s.addChildTask(task)
  s.registerTask(task, name)

  def join(): Unit = {
    s.markSleeping(Task.current, BlockReason.Join(task))
    task.await
    s.markRunning(Task.current)
    group.foreach(_.join())
  }

  def fork[T](block: Sim ?=> T)(using Sim): Fork[T] = {
    val s = summon[Sim]
    val name = s.hierarchicalTaskName + "." + s.getChildTasks.size
    Fork(name, block, Seq(this) ++ group)
  }

}

object Simulation {

  def apply[M <: ModuleInterface](
      m: M,
      timeUnit: Time,
      wave: Option[String] = None,
      debug: Boolean = false
  )(
      block: Sim ?=> M => Unit
  ): Unit = Scheduler.blocking {
    val ctrl = new SimulationController(m, timeUnit, debug, wave)
    val sim = new Simulation(ctrl, "root", m.domains.head.clock)
    given Sim = sim
    given ForkContext = ForkContext(None)

    sim.registerTask(Task.current, "root")
    try {
      block(m)
    } catch {
      case e: Throwable =>
        sim.logger.error("sim", s"Test failed: $e")
        sim.abort(e)
    }
    Logger.success(s"Simulation of ${m.name} finished")
  }

  def fork[T](block: Sim ?=> T)(using Sim): Fork[T] = {
    val s = summon[Sim]
    val name = s.hierarchicalTaskName + "." + s.getChildTasks.size
    Fork(name, block, Seq.empty)
  }

  def forkComp[T](c: Component, phase: String, block: Sim ?=> T)(using
      Sim
  ): Fork[T] = {
    val s = summon[Sim]
    val name =
      s.hierarchicalTaskName + "." + s.getChildTasks.size + s"(${c.name} in $phase)"
    Fork(name, block, Seq.empty)
  }

}

class Simulation(
    val ctrl: SimControl,
    val hierarchicalTaskName: String,
    val currentClock: ClockPort
) extends Sim {

  import SimulationController.Command.*
  import SimulationController.Response.*

  private val childTasks = collection.mutable.ListBuffer[Task[?]]()

  def withClock(c: ClockPort): Sim = {
    new Simulation(ctrl, hierarchicalTaskName, c)
  }

  def addChildTask(f: Task[?]): Unit = {
    childTasks += f
  }

  def getChildTasks: Seq[Task[?]] = childTasks.toSeq

}

object SimulationController {

  enum Command(origin: Task[?]) {
    case RegisterTask(t: Task[?], name: String) extends Command(t)
    case DeregisterThread(t: Task[?]) extends Command(t)

    case Poke(t: Task[?], p: Input[Bits], value: BigInt) extends Command(t)
    case Peek(t: Task[?], p: Port[Bits]) extends Command(t)
    case PeekMonitor(t: Task[?], p: Input[Bits]) extends Command(t)
    case Step(t: Task[?], c: ClockPort, steps: Int) extends Command(t)

    case PeekReg(t: Task[?], r: Register) extends Command(t)

    case SendToChannel[T](t: Task[?], ch: framework.Channel[T])
        extends Command(t)
    case WaitForChannel[T](t: Task[?], ch: framework.Channel[T])
        extends Command(t)

    case WaitForThread(t: Task[?], toBeJoined: Task[?]) extends Command(t)

    case Finish(t: Task[?]) extends Command(t)

    case Abort(t: Task[?], e: Throwable) extends Command(t)
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
    case WaitForMonitorRegion
  }

}

class SimulationController(
    val dut: ModuleInterface,
    timeUnit: Time,
    debug: Boolean,
    wave: Option[String]
) extends SimControl {

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

  val libPath =
    s"${p.toAbsolutePath}/build/lib${dut.name}${MakefileGenerator.libExtension}"
  val libCopy =
    s"${p.toAbsolutePath}/build/lib${dut.name}_${java.time.Instant.now().toEpochMilli}${MakefileGenerator.libExtension}"
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

  private val runningTasks = collection.mutable.Set[Task[?]]()
  private val blockReason = collection.mutable.Map[Task[?], BlockReason]()
  private val names = collection.mutable.Map[Task[?], String]()

  private val queue = InteractionQueue()

  private var finish = false
  private var abort = Option.empty[(Task[?], Throwable)]

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

  val waitingForMonitorRegion = mutable.Set[Task[?]]()

  var monitorRegion = false

  val inputDriveSkew = dut.inputs.map { p =>
    p -> 0.fs
  }.toMap

  logger.info("sim", dut.portToId.toSeq.sortBy(_._2).mkString("\n"))
  logger.info("sim", portState.toString)
  logger.info("sim", uncommitedPortState.toString)
  logger.info("sim", inputDriveSkew.toString)

  def iAmDone(): Unit = {

    logger.info("ctrl", s"""Threads:
                           |  ${runningTasks.map(t => s"- ${names(t)}: running").mkString("\n  ")}
                           |  ${blockReason.map((t, b) => s"- ${names(t)}: $b").mkString("\n  ")}""".stripMargin)

    if (runningTasks.nonEmpty) {
      // just return and leave it to the scheduler to let other tasks run
    } else if (waitingForMonitorRegion.nonEmpty) {
      monitorRegion = true
      logger.info("sim", "Entering monitor region")
      waitingForMonitorRegion.foreach { t =>
        logger.info("sim", s"Waking up task ${names(t)} in monitor region")
        t.schedule()
        runningTasks += t
      }
      waitingForMonitorRegion.clear()
    } else {

      logger.info("ctrl", "All threads sleeping")

      if (monitorRegion) {
        logger.info("ctrl", "Exiting monitor region")
      }
      monitorRegion = false

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

      iAmDone()

    }

  }

  private def handleInteraction(i: Interaction) = i match
    case Interaction.Drive(t, p, value) =>
      model.pokeInput(dut.portToId(p), value, p.width.toInt)
      uncommitedPortState(p) = false
      logger.info("sim", s"Driven $p with ${value.toString(16)}")

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

    case Interaction.Release(t, task) =>
      runningTasks += task
      task.schedule()
      logger.info("sim", s"Released ${names(task)}")

  def registerTask(t: Task[?], name: String): Unit = {
    runningTasks += t
    names(t) = name
    logger.info("sim", s"Task $name registered with id $t")
  }
  def markRunning(t: Task[?]): Unit = {
    runningTasks += t
    blockReason.remove(t)
    logger.info("sim", s"Task ${names(t)} is marked running")
  }
  def markSleeping(t: Task[?], reason: BlockReason): Unit = {
    runningTasks -= t
    blockReason(t) = reason
    logger.info("sim", s"Task ${names(t)} is marked sleeping due to $reason")
    iAmDone()
  }
  def retire(t: Task[?]): Unit = {
    runningTasks -= t
    blockReason.remove(t)
    names.remove(t)
    logger.info("sim", s"Task ${names(t)} retired")
  }
  def requestStepWakeup(t: Task[?], c: ClockPort, steps: Int): Unit = {
    val wakeup = time + c.period * steps
    queue.add(Interaction.Release(wakeup.absolute, t))
    runningTasks -= t
    blockReason(t) = BlockReason.WaitForStep(c, steps)
    logger.info(
      "sim",
      s"Task ${names(t)} wants to step $c by $steps (wake up at $wakeup)"
    )
    iAmDone()
  }
  def requestPoke(t: Task[?], p: Input[Bits], value: BigInt): Unit = {
    if (monitorRegion) {
      throw new Exception(
        "Poke should not be received when threads are running"
      )
    }
    if (uncommitedPortState.getOrElse(p, false)) {
      logger.warning("sim", s"Multiple drivers for $p")
    }
    logger.info("sim", s"Task ${names(t)} poking $p with $value")
    portState(p) = value
    uncommitedPortState(p) = true
    queue.add(
      Interaction.Drive(
        (nextNegEdge(dut.portToClockDomain(p)) + inputDriveSkew(p)).absolute,
        p,
        value
      )
    )
  }
  def requestPeek(t: Task[?], p: Port[Bits]): BigInt = {
    val v = p match {
      case Input(_)  => model.peekInput(dut.portToId(p))
      case Output(_) => model.peekOutput(dut.portToId(p), p.width.toInt)
    }
    logger.info("sim", s"Task ${names(t)} peeked $p = $v")
    v
  }
  def isInMonitorRegion(): Boolean = monitorRegion
  def requestMonitorWakeup(t: Task[?]): Unit = {
    if (monitorRegion) {
      throw new Exception("Monitor region already entered")
    }
    logger.info("sim", s"Task ${names(t)} wants to peek after everyone is done")
    waitingForMonitorRegion += t
    runningTasks -= t
    blockReason(t) = BlockReason.WaitForMonitorRegion
    logger.info("sim", s"Task ${names(t)} is waiting for monitor region")
    iAmDone()
  }

  def requestPeekMonitor(t: Task[?], p: Input[Bits]): BigInt = {
    if (!monitorRegion) {
      throw new Exception("Monitor region not entered")
    }
    val v = model.peekInput(dut.portToId(p))
    logger.info("sim", s"Task ${names(t)} peekedMonitor $p = $v")
    v
  }
  def requestPeekReg(t: Task[?], r: Register): BigInt = {
    val v = model.peekRegister(dut.regToId(r), r.w.toInt)
    logger.info("sim", s"Task ${names(t)} peeked register $r = $v")
    v
  }
  def finish(t: Task[?]): Unit = {
    logger.info("sim", s"Task ${names(t)} finished")
  }
  def abort(t: Task[?], e: Throwable): Unit = {
    logger.error("sim", s"Task ${names(t)} failed: $e")
  }

  // def run(): Unit = {
  //   logger.info("ctrl", "Waiting for command")
  //   commands.read() match {
  //     case Left(_) => logger.error("ctrl", "Unexpected command")
  //     case Right(c) =>
  //       //logger.info("ctrl", s"Handling command $c")
  //       handleCommand(c)
  //   }

  //   var monitorRegion = false

  //   while (true) {

  //     if (finish) {
  //       logger.info("ctrl", "Finishing")
  //       return
  //     }

  //     if (abort.isDefined) {
  //       val (t, e) = abort.get
  //       logger.error("ctrl", s"Aborting due to thread ${names(t)}: ${e}")
  //       return
  //     }

  //     if (threadStatus.isEmpty) {
  //       logger.info("ctrl", "All threads deregistered, exiting")
  //       return
  //     }

  //     logger.info("ctrl", s"""Threads:
  //                   |  - ${threadStatus.map((t, s) => s"${names(t)}($s) [$t]").mkString("\n  - ")} """.stripMargin)

  //     // if some are running -> handle their commands
  //     // if all are sleeping but latePeeks is not empty -> peek late
  //     // if all are sleeping -> advance time

  //     if (threadStatus.exists(_._2 == ThreadStatus.Running)) {

  //       logger.info("ctrl", "Waiting for command")
  //       commands.read() match {
  //         case Left(_) => logger.error("ctrl", "Unexpected command")
  //         case Right(c) =>
  //           logger.info("ctrl", s"Received command $c")
  //           if (monitorRegion) {
  //             c match {
  //               case Poke(t, p, value) => throw new Exception("Poke should not be received when threads are running")
  //               case _ => ()
  //             }
  //           }

  //           handleCommand(c)
  //       }

  //     } else if (latePeeks.nonEmpty) {

  //       if (!monitorRegion) {
  //         monitorRegion = true
  //         logger.info("ctrl", "entering monitor region")
  //       }

  //       logger.info("ctrl", "Peeking late")
  //       latePeeks.foreach { case (t, p) =>
  //         logger.info("ctrl", s"Peeking $p for ${names(t)}")
  //         respond(t).send(Peeked(model.peekInput(dut.portToId(p))))
  //         threadStatus(t) = ThreadStatus.Running
  //       }
  //       latePeeks.clear()

  //     } else {

  //       logger.info("ctrl", "All threads sleeping")

  //       monitorRegion = false
  //       logger.info("ctrl", "Exiting monitor region")

  //       val nextTime = queue.nextInteractionTime

  //       logger.info("ctrl", s"Next interaction at time: $nextTime")

  //       if (nextTime == time) {
  //         logger.info("ctrl", "Already at correct time")
  //       } else if (nextTime > time) {
  //         logger.info("ctrl", s"Advancing time to $nextTime")
  //         time.set(nextTime)
  //         model.tick(nextTime)
  //       } else throw new RuntimeException("Time went backwards")

  //       val interactions = queue.getInteractionsForThisTime

  //       logger.info(
  //         "ctrl",
  //         s"Handling interactions: \n  - ${interactions.mkString("  - ")}"
  //       )

  //       interactions.foreach { i =>
  //         logger.info("ctrl", s"Handling interaction $i")
  //         handleInteraction(i)
  //       }

  //     }
  //   }
  // }

  // private def handleInteraction(i: Interaction) = i match
  //   case Interaction.Drive(t, p, value) =>
  //     model.pokeInput(dut.portToId(p), value, p.width.toInt)
  //     uncommitedPortState(p) = false
  //     logger.info("cmc", s"Driven $p with ${value.toString(16)}")

  //   case Interaction.PosEdge(t, c) =>
  //     model.pokeInput(dut.portToId(c), 1, 1)
  //     val nextEdge = (t + c.period / 2).absolute
  //     queue.add(Interaction.NegEdge(nextEdge, c))
  //     nextNegEdge(dut.clockToClockDomain(c)) = nextEdge
  //     logger.info("sim", s"Posedge $c")

  //   case Interaction.NegEdge(t, c) =>
  //     model.pokeInput(dut.portToId(c), 0, 1)
  //     val nextEdge = (t + c.period / 2).absolute
  //     queue.add(Interaction.PosEdge(nextEdge, c))
  //     logger.info("sim", s"Negedge $c")

  //   case Interaction.Release(t, thread) =>
  //     threadStatus(thread) = ThreadStatus.Running
  //     respond(thread).send(Stepped)
  //     logger.info("sim", s"Released ${names(thread)}")

  // private def handleCommand(c: Command)(using Async) = c match
  //   case RegisterThread(t, name, response) =>
  //     threadStatus(t) = ThreadStatus.Running
  //     respond(t) = response
  //     names(t) = name
  //     logger.info("cmd", s"Registered thread $name")

  //   case DeregisterThread(t) =>
  //     logger.info("cmd", s"Deregistered thread ${names(t)}")

  //     if (joins.keys.toSeq.contains(t)) {
  //       logger.info("cmd", s"Thread ${names(t)} has threads waiting for it")
  //       val waitingThreads = joins(t)
  //       waitingThreads.foreach { wt =>
  //         logger.info("cmd", s"Waking up thread ${names(wt)}")
  //         respond(wt).send(Joined)
  //         threadStatus(wt) = ThreadStatus.Running
  //       }
  //       joins.remove(t)
  //     }
  //     threadStatus.remove(t)
  //     respond.remove(t)

  //   case Poke(t, p, value) =>
  //     if (uncommitedPortState(p))
  //       logger.warning("cmd", s"Multiple drivers for $p")
  //     logger.info("cmd", s"Thread ${names(t)} poking $p with $value")
  //     portState(p) = value
  //     uncommitedPortState(p) = true
  //     queue.add(
  //       Interaction.Drive(
  //         (nextNegEdge(dut.portToClockDomain(p)) + inputDriveSkew(p)).absolute,
  //         p,
  //         value
  //       )
  //     )

  //   case Peek(t, p) =>
  //     val v = p match
  //       case Input(_)  =>
  //         model.peekInput(dut.portToId(p))
  //       case Output(_) => model.peekOutput(dut.portToId(p), p.width.toInt)
  //     logger.info("cmd", s"Thread ${names(t)} peeked $p = $v")
  //     respond(t).send(Peeked(v))

  //   case PeekMonitor(t, p) =>
  //     latePeeks.addOne(t -> p)
  //     logger.info("cmd", s"Thread ${names(t)} want to peek $p after everyone is done")
  //     threadStatus(t) = ThreadStatus.WaitForMonitorRegion

  //   case PeekReg(t, r) =>
  //     val v = model.peekRegister(dut.regToId(r), r.w.toInt)
  //     logger.info("cmd", s"Thread ${names(t)} peeked register $r = $v")
  //     respond(t).send(Peeked(v))

  //   case Step(t, c, steps) =>
  //     val wakeup = time + c.period * steps
  //     queue.add(Interaction.Release(wakeup.absolute, t))
  //     threadStatus(t) = ThreadStatus.WaitForStep
  //     logger.info("cmd", s"Thread ${names(t)} want to step $c by $steps (wake up at $wakeup)")

  //   case SendToChannel(t, ch) =>
  //     logger.info("cmd", s"Thread ${names(t)} sent to channel")
  //     if (reads.contains(ch)) {

  //       val r = reads(ch)
  //       logger.info("cmd", s"Channel has already ${names(r)} someone waiting")
  //       reads.remove(ch)
  //       threadStatus(r) = ThreadStatus.Running
  //     } else {
  //       sends(ch) = t
  //       threadStatus(t) = ThreadStatus.BlockedSend
  //       logger.info("cmd", s"Channel has no one waiting. Marked thread ${names(t)} as sleeping")
  //     }

  //   case WaitForChannel(t, ch) =>
  //     logger.info("cmd", s"Thread ${names(t)} waiting for channel")
  //     if (sends.contains(ch)) {
  //       logger.info("cmd", s"Channel has already someone sending")
  //       val s = sends(ch)
  //       sends.remove(ch)
  //       threadStatus(s) = ThreadStatus.Running
  //     } else {
  //       reads(ch) = t
  //       threadStatus(t) = ThreadStatus.BlockedRead
  //       logger.info("cmd", s"Channel has no one sending. Marked thread ${names(t)} as sleeping")
  //     }

  //   case WaitForThread(t, toBeJoined) =>
  //     logger.info("cmd", s"Thread ${names(t)} wants to wait for thread ${names(toBeJoined)}")

  //     if (threadStatus.keys.toSeq.contains(toBeJoined)) {
  //       logger.info("cmd", s"Thread ${names(toBeJoined)} still running. Marked thread ${names(t)} as sleeping")
  //       threadStatus(t) = ThreadStatus.JoinBlocked(names(toBeJoined))
  //       joins.getOrElseUpdate(toBeJoined, collection.mutable.ListBuffer()).addOne(t)
  //     } else {
  //       logger.info("cmd", s"Thread ${names(toBeJoined)} has already stopped. Continuing thread ${names(t)}")
  //       respond(t).send(Joined)
  //     }

  //   case Finish(t) => finish = true
  //   case Abort(t, e) => abort = Some(t -> e)
}
