import framework.*
import framework.given
import types.*
import Time.*

class CDC extends ModuleInterface("src/hdl/sv/CDC.sv") {

  val clk_l = ClockPort(6.ns)
  val clk_r = ClockPort(10.ns)

  val rst_l = ResetPort()
  val rst_r = ResetPort()

  val req_l = Input(Bool())
  val req_r = Output(Bool())

  val ack_l = Output(Bool())
  val ack_r = Input(Bool())

  val data_l = Input(UInt(32.W))
  val data_r = Output(UInt(32.W))

  domain(clk_l, rst_l)(
    req_l,
    data_l,
    ack_l
  )

  domain(clk_r, rst_r)(
    req_r,
    data_r,
    ack_r
  )

}

class HandshakeSender(
    clock: ClockPort,
    reset: ResetPort,
    req: Input[Bool],
    ack: Output[Bool],
    data: Input[UInt]
) {

  def reset()(using Sim, Async): Unit = {
    reset.assert()
    clock.step()
    reset.deassert()
  }

  def send(value: BigInt)(using Sim, Async): Unit = {
    req.poke(true)
    data.poke(value)
    clock.stepUntil(ack.peek)
    req.poke(false)
    data.poke(0)
    clock.stepUntil(!ack.peek)
  }
}

class HandshakeReceiver(
    clock: ClockPort,
    reset: ResetPort,
    req: Output[Bool],
    ack: Input[Bool],
    data: Output[UInt]
) {

  def reset()(using Sim, Async): Unit = {
    reset.assert()
    clock.step()
    reset.deassert()
  }

  def expect(value: BigInt)(using Sim, Async): Unit = {
    clock.stepUntil(req.peek)
    data.expect(value)
    ack.poke(true)
    clock.stepUntil(!req.peek)
    ack.poke(false)
  }
}

@main def CdcTest(): Unit =
  Simulation(CDC(), 1.ns, Some("cdc.vcd"),debug = true) { cdc =>
    val magicNum = BigInt("deadbeef", 16)
    fork {
      val sender =
        HandshakeSender(cdc.clk_l, cdc.rst_l, cdc.req_l, cdc.ack_l, cdc.data_l)
      sender.reset()
      sender.send(magicNum)
    }.fork {
      val receiver = HandshakeReceiver(cdc.clk_r, cdc.rst_r, cdc.req_r, cdc.ack_r, cdc.data_r)
      receiver.reset()
      receiver.expect(magicNum)
    }.join()
  }
