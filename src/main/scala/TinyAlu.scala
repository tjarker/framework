
import framework.*
import types.*
import Time.*
import framework.given

import gears.async.Async
import scala.util.boundary

class TinyAlu extends Module("src/hdl/sv/tinyalu.sv") {

  override val name = "tinyalu"

  val clk = ClockPort(2.ps)
  val reset_n = ResetPort()

  val start = Input(Bool())

  val a = Input("A", UInt(8.W))
  val b = Input("B", UInt(8.W))
  val op = Input(UInt(3.W))

  val done = Output(Bool())
  val result = Output(UInt(8.W))

  domain(clk, reset_n)(
    start, a, b, op, done, result
  )
}

object TinyAlu {

  enum Op(val enc: Int) {
    case Add extends Op(1)
    case And extends Op(2)
    case Xor extends Op(3)
    case Mul extends Op(4)
  }

  def prediction(a: BigInt, b: BigInt, op: Op): BigInt = {
    op match {
      case Op.Add => a + b
      case Op.And => a & b
      case Op.Xor => a ^ b
      case Op.Mul => a * b
    }
  }


}

class TinyAluBfm(dut: TinyAlu) {

  def reset()(using Sim, Async) = {
    dut.reset_n.deassert()
    dut.start.poke(false)
    dut.clk.step()
    dut.reset_n.assert()
    dut.clk.step()
  }

  def sendOp(a: BigInt, b: BigInt, op: TinyAlu.Op)(using Sim, Async.Spawn): Unit = fork {
    dut.a.poke(a)
    dut.b.poke(b)
    dut.op.poke(op.enc)
    dut.start.poke(true)
    dut.clk.step()
    dut.start.poke(false)
  }

}

@main def TinyAluTest(): Unit = Simulation(TinyAlu(), 1.ps) { dut =>

  val bfm = TinyAluBfm(dut)

  bfm.reset()

  for {
    op <- TinyAlu.Op.values
  } {
    val a = BigInt(8, scala.util.Random)
    val b = BigInt(8, scala.util.Random)
    val prediction = TinyAlu.prediction(a, b, op)
    bfm.sendOp(a, b, op)
    dut.clk.stepUntil(dut.done.peek)
    dut.result.expect(prediction)

    dut.clk.step()
  }

}
