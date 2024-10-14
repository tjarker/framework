package framework

import collection.mutable

import Time.*
import types.*
import Module.*

object SimController {

  

}

class SimController(dut: Module, timeUnit: Time) {


  enum Event(val timestamp: AbsoluteTime) {
    case Drive(t: AbsoluteTime, p: Input[Bits], value: BigInt) extends Event(t)
    case PosEdge(t: AbsoluteTime, cd: ClockDomain) extends Event(t)
    case NegEdge(t: AbsoluteTime, cd: ClockDomain) extends Event(t)

    override def toString(): String = {
      this match {
        case Drive(t, p, value) => s"${p.name} = $value"
        case PosEdge(t, cd)     => s"${cd.clock.name} up"
        case NegEdge(t, cd)     => s"${cd.clock.name} down"
      }
    }
  }

  import SimController.*

  dut.ctrl = this

  val simTime: SimulationTime = SimulationTime(() => doTick)

  val sim =
    VerilatorInterface(
      dut.libPath,
      dut.name,
      "wave/" + dut.name + ".vcd",
      timeUnit
    )

  val scheduler = Scheduler(simTime, doTick)
  scheduler.addMainThread()

  // clock edge and drive events
  val events = mutable.PriorityQueue[Event]()(
    Ordering.by(e =>
      (
        -e.timestamp.fs,
        e match {
          case _: Event.Drive => 1
          case _              => 0
        }
      )
    )
  )

  // used to schedule drive events, since they are scheduled
  // relative to the next negative edge of their clock domain
  val nextNegEdge = mutable.Map[ClockDomain, AbsoluteTime]()

  val driveSkewTable = dut.inputs.map { i =>
    i -> i.driveSkew / timeUnit
  }.toMap

  val state = mutable.Map[Port[Bits], BigInt]()
  dut.ports.foreach { p =>
    state(p) = 0
  }

  // simulation starts at a negative edge
  dut.domains.foreach { cd =>
    nextNegEdge(cd) = 0.fs.absolute
  }

  // schedule the first positive edge for each clock domain
  dut.domains.foreach { cd =>
    events.enqueue(Event.PosEdge(cd.halfPeriod.absolute, cd))
  }

  println(s"SimController for ${dut.name} created")
  println(s"Ports:")
  println(
    dut.ports.zipWithIndex
      .map { case (p, i) => s"  $i: $p" }
      .mkString("\n")
  )

  // ==========================================================================
  // Public API

  def peek(p: Port[Bits]): BigInt = {
    println(s"[Ctrl] peeking ${p.name}")
    p match
      case Input(t)  => sim.peekInput(dut.portToId(p))
      case Output(t) => sim.peekOutput(dut.portToId(p))
  }

  def poke(p: Input[Bits], value: BigInt): Unit = {

    val cd = dut.portToClockDomain(p)

    val signalChangeTick = nextNegEdge(cd) + p.driveSkew

    events.enqueue(Event.Drive(signalChangeTick.absolute, p, value))

    // TODO: add multi drive detection in same tick (could be done by having a dirty bit map for ports)
  }

  def step(clock: Input[Clock], steps: Int): Unit = {

    val cd = dut.portToClockDomain(clock)

    println(s"[Ctrl] stepping ${steps} ticks on ${cd.clock.name}")

    scheduler.sleepUntil((simTime + cd.period * steps).absolute)
  }

  def tick(t: Time): Unit = {

    scheduler.sleepUntil((simTime + t).absolute)
  }

  // ==========================================================================

  // ==========================================================================
  // Private methods

  // apply the action associated with the evnt (clock edge or drive)
  private def executeEvent(e: Event): Unit = {
    e match {
      case Event.Drive(t, p, value) => sim.pokeInput(dut.portToId(p), value)

      case Event.PosEdge(t, cd) =>
        sim.pokeInput(dut.portToId(cd.clock), 1)
        events.enqueue(
          Event.NegEdge((simTime + cd.halfPeriod).absolute, cd)
        )
        nextNegEdge(cd) = (simTime + cd.halfPeriod).absolute

      case Event.NegEdge(t, cd) =>
        sim.pokeInput(dut.portToId(cd.clock), 0)
        events.enqueue(
          Event.PosEdge((simTime + cd.halfPeriod).absolute, cd)
        )
    }
  }

  // advance simulation time to the target tick
  // applying all scheduled events along the way
  private def doTick(until: AbsoluteTime) = {

    println(s"[Ctrl] executing until ${until}")

    while (events.nonEmpty && events.head.timestamp <= until) {

      println(s"Queued events:")
      println(
        events
          .map {
            case Event.Drive(t, p, value) => s"  $t: ${p.name} = $value"
            case Event.PosEdge(t, cd)     => s"  $t: ${cd.clock.name} up"
            case Event.NegEdge(t, cd)     => s"  $t: ${cd.clock.name} down"
          }
          .mkString("\n")
      )

      val e = events.dequeue()

      if (e.timestamp < simTime)
        throw new Exception("Event timestamp is in the past")

      if (e.timestamp > simTime) {
        sim.tick(e.timestamp)
        dut.time.set(e.timestamp)
        println(s"@${dut.time}")
      }

      if (simTime > 1000.ns) throw new Exception("Timeout")

      println(e)
      executeEvent(e)
    }
  }

}
