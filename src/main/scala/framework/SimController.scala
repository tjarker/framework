package framework

import collection.mutable

import Time.*
import Types.*
import Module.*


object SimController {

  type Tick = Long

  enum Event(val timestamp: Long) {
    case Drive(t: Tick, p: Input[Bits], value: BigInt) extends Event(t)
    case PosEdge(t: Tick, cd: ClockDomain) extends Event(t)
    case NegEdge(t: Tick, cd: ClockDomain) extends Event(t)
  }

}

class SimController(dut: Module) {

  import SimController.*

  val timeUnit = 1.ns
  var currentSimTick: Tick = 0

  extension (c: Input[Clock]) {
    def periodInTicks: Tick = c.period / timeUnit
  }

  val sim =
    VerilatorInterface(
      dut.libPath,
      dut.name,
      "wave/" + dut.name + ".vcd",
      timeUnit
    )

  val events =
    mutable.PriorityQueue[Event]()(Ordering.by(-_.timestamp))

  val nextNegEdge = mutable.Map[ClockDomain, Tick]()
  dut.domains.foreach { cd =>
    nextNegEdge(cd) = 0
  }

  dut.ctrl = this

  dut.domains.foreach { cd =>
    events.enqueue(Event.PosEdge(cd.clock.periodInTicks / 2, cd))
  }

  println(s"SimController for ${dut.name} created")
  println(dut.domains.map(_.toString()).mkString("\n"))
  println(dut.domains.head.ports.zipWithIndex.map { case (p, i) => s"$i: $p" }.mkString("\n"))

  def peek(p: Port[Bits]): BigInt = {
    p match
      case Input(t)  => sim.peekInput(dut.portToId(p))
      case Output(t) => sim.peekOutput(dut.portToId(p))

  }

  def executeEvent(e: Event): Unit = {
    e match {
      case Event.Drive(t, p, value) => sim.pokeInput(dut.portToId(p), value)
      case Event.PosEdge(t, cd) => 
        sim.pokeInput(dut.portToId(cd.clock), 1)
        events.enqueue(Event.NegEdge(currentSimTick + cd.clock.periodInTicks / 2, cd))
        nextNegEdge(cd) = currentSimTick + cd.clock.periodInTicks / 2
      case Event.NegEdge(t, cd) => 
        sim.pokeInput(dut.portToId(cd.clock), 0)
        events.enqueue(Event.PosEdge(currentSimTick + cd.clock.periodInTicks / 2, cd))
    }
  }

  def poke(p: Input[Bits], value: BigInt): Unit = {

    val cd = dut.portToClockDomain(p)

    val signalChangeTick = nextNegEdge(cd) + p.driveSkew / timeUnit
    
    val e = Event.Drive(signalChangeTick, p, value)

    println(s"Enqueuing event: $e")

    events.enqueue(e)

    // TODO: add multi drive detection in same tick (could be done by having a dirty bit map for ports)
  }

  def doTick(ticks: Long) = {

    println(s"doTick($ticks)")

    println(s"events: ${events.mkString(", ")}")

    val targetTick = currentSimTick + ticks

    while (events.nonEmpty && events.head.timestamp <= targetTick) {
      val e = events.dequeue()
      println(s"Executing event: $e")

      if (e.timestamp < currentSimTick) throw new Exception("Event timestamp is in the past")

      if (e.timestamp > currentSimTick) {
        sim.tick((e.timestamp - currentSimTick).toInt)
        dut.time.inc((e.timestamp - currentSimTick).fs)
        println(s"had to advance time to ${e.timestamp}")
        currentSimTick = e.timestamp
      }

      if (currentSimTick > 800) throw new Exception("Timeout")

      executeEvent(e)
    }
  }

  def tick(t: Time): Unit = {
    if (t < timeUnit) throw Exception("Time must be greater than time unit")

    val ticks = (t / timeUnit).toInt

    doTick(ticks)
  }

  def step(cd: Input[Clock], steps: Int): Unit = {
    if (dut.time > timeUnit * 100) throw new Exception("Timeout")

    println(s"Stepping $steps ticks ${cd.periodInTicks}")
    //sim.stepClockDomain(dut.clockToClockDomain(cd), steps)
    doTick(cd.periodInTicks * steps)
    // printState()
  }

  def printState(): Unit = {
    println("State:")
    dut.ports.foreach { p =>
      println(s"${p}: ${peek(p)}")
    }
  }

}
