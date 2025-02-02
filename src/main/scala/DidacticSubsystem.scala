import framework.*
import framework.types.*
import framework.Time.ns

import gears.async.Async
import gears.async.Async.Spawn

class toplevel(aw: Int = 10, dw: Int = 32)
    extends ModuleInterface(
      "src/hdl/sv/student_ss_1.sv",
      "src/hdl/sv/toplevel.sv"
    ) {

  val clk = ClockPort("clk_in", 2.ns)
  val reset = ResetPort("reset_int")

  val addr = Input("PADDR", UInt(aw.W))
  val en = Input("PENABLE", Bool())
  val sel = Input("PSEL", Bool())
  val wdata = Input("PWDATA", UInt(dw.W))
  val wr = Input("PWRITE", Bool())
  val rdata = Output("PRDATA", UInt(dw.W))
  val ready = Output("PREADY", Bool())
  val slverr = Output("PSLVERR", Bool())

  val irq = Output(Bool())

  val irqEn = Input("irq_en", Bool())
  val ssCtrl = Input("ss_ctrl", UInt(8.W))

  val pmod0Gpi = Input("pmod_0_gpi", UInt(4.W))
  val pmod0Gpo = Output("pmod_0_gpo", UInt(4.W))
  val pmod0Oe = Output("pmod_0_gpio_oe", UInt(4.W))

  val pmod1Gpi = Input("pmod_1_gpi", UInt(4.W))
  val pmod1Gpo = Output("pmod_1_gpo", UInt(4.W))
  val pmod1Oe = Output("pmod_1_gpio_oe", UInt(4.W))

  domain(clk, reset)(
    addr,
    en,
    sel,
    wdata,
    wr,
    rdata,
    ready,
    slverr,
    irq,
    irqEn,
    ssCtrl,
    pmod0Gpi,
    pmod0Gpo,
    pmod0Oe,
    pmod1Gpi,
    pmod1Gpo,
    pmod1Oe
  )

}

import apb.*

class DidacticTest(apb: ApbBfm)(using Hierarchy) extends Test, ResetPhase {

  Config.set("bfm" -> apb)
  Factory.overrideType[ApbBaseSeq, ApbRandomSeq]

  val env = Factory.create[ApbEnv]

  def reset()(using Sim, Async.Spawn) = {
    apb.reset()
  }

  def test()(using Sim, Async.Spawn): Unit = {

    val seq = Factory.create[ApbBaseSeq]
    seq.start()

    env.agent.seq.play(seq)

    info("waiting for sequence to finish")
    seq.waitUntilDone()

    info("Test done")

  }

}

@main def DidacticSocTest(): Unit = Test.run(toplevel(10, 32), 1.ns, Some("didactic.vcd")) { dut =>

  val apb = ApbBfm(
    dut.clk,
    dut.reset,
    dut.addr,
    dut.en,
    dut.sel,
    dut.wdata,
    dut.wr,
    dut.rdata,
    dut.ready,
    dut.slverr
  )

  util.Random.setSeed(42)

  val t = DidacticTest(apb)

  println("Running test")
  t
}
