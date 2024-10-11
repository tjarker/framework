import com.sun.jna.Native

import scala.collection.mutable

import Time._

import java.nio.file.Path

object NewGcdSim extends App {

  case class Width(width: Int) {
    override def toString(): String = s"${width - 1}:0"
  }

  extension (w: Int) {
    def W: Width = Width(w)
  }

  trait Bits {
    def width: Width
  }

  trait Data extends Bits

  class UInt(val width: Width) extends Data {
    override def toString(): String = s"UInt($width)"
  }

  class Bool() extends Data {
    val width = 1.W
    override def toString(): String = "Bool"
  }

  class SInt(val width: Width) extends Data {
    override def toString(): String = s"SInt($width)"
  }

  class Clock() extends Bits {
    val width = 1.W
    override def toString(): String = "Clock"
  }

  class Reset() extends Bits {
    val width = 1.W
    override def toString(): String = "Reset"
  }

  trait Port[+T <: Bits] {
    def width: Width
    val ctx: ModuleBuilderContext
  }

  class Output[+T <: Bits](val t: T)(using val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString(): String = s"Output($t)"
  }
  object Output {
    def unapply[T <: Bits](o: Output[T]): Option[T] = Some(o.t)
  }

  class Input[+T <: Bits](val t: T)(using val ctx: ModuleBuilderContext)
      extends Port[T] {
    val width = t.width

    override def toString: String = s"Input($t)"
  }
  object Input {
    def unapply[T <: Bits](i: Input[T]): Option[T] = Some(i.t)
  }

  extension [T <: Data](p: Port[T]) {
    def peek: BigInt = {
      p.ctx.peek.apply().apply(p)
    }
  }

  extension [T <: Data](p: Input[T]) {
    def poke(value: BigInt): Unit = {
      p.ctx.poke.apply().apply(p, value)
    }
  }

  extension (p: Input[Clock]) {
    def step(steps: Int): Unit = {
      p.ctx.step.apply().apply(p, steps)
    }
  }

  extension (p: Input[Reset]) {
    def assert(): Unit = {
      p.ctx.poke.apply().apply(p, 1)
    }
    def deassert(): Unit = {
      p.ctx.poke.apply().apply(p, 0)
    }
    def peek: BigInt = {
      p.ctx.peek.apply().apply(p)
    }
  }

  case class ClockDomain(
      val clock: Port[Clock],
      val reset: Option[Port[Reset]],
      val inputs: mutable.ArrayBuffer[Input[Bits]],
      val outputs: mutable.ArrayBuffer[Output[Bits]]
  ) {
    override def toString(): String = {
      s"ClockDomain(${clock}, ${reset}, ${inputs.mkString("[",", ","]")}, ${outputs.mkString("[",", ","]")})"
    }
  }

  class ModuleBuilderContext(
      val peek: () => (Port[Bits] => BigInt),
      val poke: () => ((Input[Bits], BigInt) => Unit),
      val step: () => ((Input[Clock], Int) => Unit)
  )

  trait Module(val libPath: String) {

    val name: String

    val domains = mutable.ArrayBuffer[ClockDomain]()

    lazy val portToId = domains.flatMap { cd =>
      (cd.inputs ++ cd.outputs).zipWithIndex
    }.toMap

    lazy val idToPort = portToId.map(_.swap)

    lazy val clockToClockDomain = domains.map { cd =>
      cd.clock -> cd
    }.toMap

    var ctrl: SimController = null
    given ModuleBuilderContext = ModuleBuilderContext(
      () => ctrl.peek,
      () => ctrl.poke,
      () => ctrl.step
    )

    def tick(t: Time): Unit = {
      ctrl.tick(t)
    }

  }

  trait Scheduler {}

  class VerilatorInterface(
      libPath: String,
      dutName: String,
      waveFile: String,
      timeUnit: Time
  ) {

    // TODO: caching should happen here!

    val lib = Native.load(libPath, classOf[ModelInterface])

    val ctx = lib.createSimContext(
      dutName + "\u0000",
      waveFile + "\u0000",
      timeUnit.toString + "\u0000"
    )

    def tick(n: Int = 1) = for (_ <- 0 until n) lib.tick(ctx)

    def setInput(id: Long, value: Long) = lib.setInput(ctx, id, value)

    def getOutput(id: Long) = lib.getOutput(ctx, id)

    def destroy() = lib.destroySimContext(ctx)

    def peekInput(id: Long): BigInt = ???

    def pokeInput(id: Long, value: BigInt): Unit = setInput(id, value.toLong)

    def peekOutput(id: Long): BigInt = getOutput(id)

    def stepClockDomain(cd: ClockDomain, steps: Int): Unit = println(s"Step $steps in $cd")

  }

  class SimController(dut: Module) {

    val timeUnit = 1.ns

    val sim = VerilatorInterface(dut.libPath, dut.name, dut.name + ".vcd", timeUnit)

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
      sim.stepClockDomain(dut.clockToClockDomain(cd), steps)
    }

  }

  object SimController {
    def apply[M <: Module](dut: M): M = {
      val ctrl = new SimController(dut)
      dut
    }
  }

  class Gcd extends Module("./gcd/build/gcd_model.so") {

    val name = "GCD"

    val clock = Input(Clock())
    val reset = Input(Reset())
    val req = Input(Bool())
    val ack = Output(Bool())
    val loadVal = Input(UInt(16.W))
    val result = Output(UInt(16.W))

    domains += ClockDomain(
      clock,
      Some(reset),
      mutable.ArrayBuffer[Input[Bits]](reset, req, loadVal),
      mutable.ArrayBuffer[Output[Bits]](ack, result)
    )
  }

  val gcd = SimController(Gcd())

  gcd.clock.step(2)
  gcd.reset.assert()
  gcd.req.poke(0)
  gcd.loadVal.poke(16)
  gcd.clock.step(2)
  gcd.reset.deassert()
  gcd.clock.step(2)

  gcd.tick(2.ns + 10.ps)

}
