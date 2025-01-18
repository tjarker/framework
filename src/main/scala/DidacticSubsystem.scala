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


class DidacticTest(apb: ApbBfm) extends Component, ResetPhase, TestPhase {

  val driver = ApbProducerDriver(apb)
  val sequencer = framework.Sequencer(driver.txChan)

  def reset()(using Sim, Async.Spawn) = {
    apb.reset()
  }

  def test()(using Sim, Async.Spawn): Unit = {


    val seq = ApbRandomSeq(10)
    

    sequencer.play(seq)

    seq.start()

    apb.clk.step(4)

  }

}

@main def DidactivUVM(): Unit = {

  Simulation(toplevel(10,32), 1.ns, false) { dut =>

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

    val test = Component.root(DidacticTest(apb))

    Phase.run(test)
    Phase.reset(test)
    Phase.test(test)
    Phase.report(test)

  }
}

@main def DidacticSim(): Unit = {


  Simulation(toplevel(10,32), 1.ns) { dut =>

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

    dut.pmod1Gpi.poke(0xF)
    
    apb.reset()
    
    println(apb.write(0x0, 1234))
    println(apb.write(0x0, 5678))

    println("0x0: " + apb.read(0x0))

    println("0x4: " + apb.read(0x4))

  }

}
