
import framework.*
import framework.Types.*
import framework.Time.*

import scala.collection.mutable

@main def CdcSim(): Unit = {

  class Cdc extends Module("./cdc/build/libcdc.so") {

    val name = "CDC"

    val clk_l = Input(Clock(6.ns))
    val clk_r = Input(Clock(10.ns))

    val rst_l = Input(Reset())
    val rst_r = Input(Reset())

    val req_l = Input(Bool())
    val req_r = Output(Bool())

    val ack_l = Output(Bool())
    val ack_r = Input(Bool())

    val data_l = Input(UInt(16.W))
    val data_r = Output(UInt(16.W))

    domains += ClockDomain(
      clk_l,
      Some(rst_l),
      mutable.ArrayBuffer[Input[Bits]](req_l, data_l),
      mutable.ArrayBuffer[Output[Bits]](ack_l)
    )

    domains += ClockDomain(
      clk_r,
      Some(rst_r),
      mutable.ArrayBuffer[Input[Bits]](ack_r),
      mutable.ArrayBuffer[Output[Bits]](req_r, data_r)
    )
  }

  val cdc = simulate(Cdc(), 1.ns)

  println(cdc.domains.mkString("\n"))

  println(cdc.ports.mkString("\n"))

  cdc.ctrl.scheduler.addThread("left") {

    cdc.rst_l.assert()
    cdc.req_l.poke(0)
    cdc.data_l.poke(0)

    cdc.clk_l.step()

    cdc.rst_l.deassert()

    cdc.clk_l.step()

    cdc.req_l.poke(1)
    cdc.data_l.poke(42)

    while (cdc.ack_l.peek == 0) {
      cdc.clk_l.step()
    }

    cdc.req_l.poke(0)
    cdc.data_l.poke(0)

    while (cdc.ack_l.peek != 0) {
      cdc.clk_l.step()
    }

  }

  cdc.ctrl.scheduler.addThread("right") {

    cdc.rst_r.assert()
    cdc.ack_r.poke(0)

    cdc.clk_r.step()

    cdc.rst_r.deassert()

    cdc.clk_r.step()

    while (cdc.req_r.peek == 0) {
      cdc.clk_r.step()
    }

    val res = cdc.data_r.peek

    cdc.clk_r.step()

    cdc.ack_r.poke(1)

    while (cdc.req_r.peek != 0) {
      cdc.clk_r.step()
    }

    cdc.ack_r.poke(0)

  }

  cdc.clk_r.step(15)
  cdc.ctrl.scheduler.joinAll()

}
