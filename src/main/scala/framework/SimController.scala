package framework

import Time.*
import Types.*
import Module.*

class SimController(dut: Module) {

  val timeUnit = 1.ns

  val sim =
    VerilatorInterface(dut.libPath, dut.name, "wave/" + dut.name + ".vcd", timeUnit)

  dut.ctrl = this

  println(s"SimController for ${dut.name} created")
  println(dut.domains.map(_.toString()).mkString("\n"))
  println(dut.portToId)

  def peek(p: Port[Bits]): BigInt = {
    p match
      case Input(t)  => sim.peekInput(dut.portToId(p))
      case Output(t) => sim.peekOutput(dut.portToId(p))

  }

  def poke(p: Input[Bits], value: BigInt): Unit = {
    sim.pokeInput(dut.portToId(p), value)
  }

  def tick(t: Time): Unit = {
    if (t < timeUnit) throw Exception("Time must be greater than time unit")
    else sim.tick(t.valueIn(timeUnit).toInt)

  }

  def step(cd: Input[Clock], steps: Int): Unit = {
    if (dut.time > timeUnit * 1000) throw new Exception("Timeout")
    sim.stepClockDomain(dut.clockToClockDomain(cd), steps)
    dut.time.inc(timeUnit * steps)
    //printState()
  }

  def printState(): Unit = {
    println("State:")
    dut.ports.foreach { p =>
      println(s"${p}: ${peek(p)}")
    }
  }

}
