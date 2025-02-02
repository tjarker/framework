import framework.*
import framework.given
import types.*
import Time.*

class GCD extends ModuleInterface("src/hdl/sv/GCD.sv") {

  val clock = ClockPort(10.ns)
  val reset = ResetPort()
  val req = Input(Bool())
  val ack = Output(Bool())
  val loadVal = Input(UInt(32.W))
  val result = Output(UInt(32.W))

  domain(clock, reset)(
    req,
    ack,
    loadVal,
    result
  )

  val a = Reg(32.W, "a")
  val b = Reg(32.W, "b")
  val state = Reg(3.W, "state")

}

class GcdBfm(gcd: GCD) {

  val states = Seq("wait_a", "ack_a", "wait_b", "compare", "update_a", "update_b", "ack_result")

  def printState()(using Sim, Async): Unit = {
    println(s"@${gcd.time}${"=" * 80}")
    println(s"State: ${states(gcd.state.peekReg.toInt)}")
    println(s"A = ${gcd.a.peekReg} B = ${gcd.b.peekReg}")
  }

  def transact(value: BigInt, expected: Option[BigInt])(using
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

  def calc(nums: (BigInt, BigInt))(using Sim, Async): BigInt = {
    transact(nums._1, None)
    transact(nums._2, Some(model(nums)))
  }

  def model(nums: (BigInt, BigInt)): BigInt = {
    nums._1.gcd(nums._2)
  }

  def reset()(using Sim, Async): Unit = {
    gcd.reset.assert()
    gcd.req.poke(false)
    gcd.loadVal.poke(0)
    gcd.clock.step()
    gcd.reset.deassert()
    gcd.clock.step()
  }

  def step(n: Int = 1)(using Sim, Async): Unit = {
    gcd.clock.step(n)

  }
}

@main def GcdTest(): Unit =
  Simulation(new GCD, 1.ns, Some("gcd.vcd")) { gcd =>
    val bfm = GcdBfm(gcd)

    fork {
      while (true) {
        bfm.printState()
        bfm.step()
      }
    }

    val tests = Seq(
      BigInt(12) -> BigInt(3),
      BigInt("6789AC", 16) -> BigInt("56789A", 16) 
    )
    bfm.reset()
    tests.foreach { test =>
      val res = bfm.calc(test)
      println(s"Result: ${res}")
    }
  }
