import com.sun.jna.Native

import scala.collection.mutable

import gears.async.*

import framework.*
import framework.given
import types.*
import Time.*

class GCD extends ModuleInterface("src/hdl/sv/GCD.sv") {

  val clock = ClockPort(10.ns)
  val reset = ResetPort()
  val req = Input(Bool())
  val ack = Output(Bool())
  val loadVal = Input(UInt(128.W))
  val result = Output(UInt(128.W))

  domain(clock, reset)(
    req,
    ack,
    loadVal,
    result
  )

  val a = Reg(128.W, "a")
  val b = Reg(128.W, "b")
  val state = Reg(3.W, "state")
  val helloA = Reg(7.W, "hello.a")

}

@main def GcdSim(): Unit = {

  def printState(gcd: GCD)(using Sim, Async): Unit = {
    val s = summon[Sim]
    println(s"@${gcd.time}${"=" * 80}")
    println(s"State: ${s.peekReg(gcd.state)}")
    println(s"Regs: ${s.peekReg(gcd.a)} ${s.peekReg(gcd.b)}")
    println(s"hello.a: ${s.peekReg(gcd.helloA).toString(2)}")
  }

  def transact(gcd: GCD, value: BigInt, expected: Option[BigInt])(using
      Sim,
      Async
  ): BigInt = {

    gcd.loadVal.poke(value)
    gcd.req.poke(true)

    gcd.clock.stepUntil(gcd.ack.peek)


    val res = gcd.result.peek
    expected.foreach { e =>
      gcd.result.expect(e)
    }

    gcd.req.poke(false)

    gcd.clock.stepUntil(!gcd.ack.peek)

    res
  }

  def calc(gcd: GCD, nums: (BigInt, BigInt))(using Sim, Async): BigInt = {
    transact(gcd, nums._1, None)
    transact(gcd, nums._2, Some(model(nums)))
  }

  def model(nums: (BigInt, BigInt)): BigInt = {
    nums._1.gcd(nums._2)
  }

  Simulation(GCD(), 1.ns) { gcd =>

    fork {
      while(true) {
        printState(gcd)
        gcd.clock.step()
      }
    }

    gcd.reset.assert()
    gcd.req.poke(false)
    gcd.loadVal.poke(0)

    gcd.clock.step()

    gcd.reset.deassert()

    gcd.clock.step()

    // throw new Exception("Not implemented")

    val large = BigInt("6789ABCDEF1", 16) -> BigInt("56789ABCDEF12", 16)
    val small = BigInt(12) -> BigInt(3)

    val res = calc(gcd, small)

    println(s"Result: ${res}")

  }

}
