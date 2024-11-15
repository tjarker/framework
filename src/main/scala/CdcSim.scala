import framework.*
import types.*
import Time.*

import scala.collection.mutable

@main def CdcSim(): Unit = {

  class CDC extends Module("./cdc/build/libcdc.so") {

    val clk_l = ClockPort(6.ns)
    val clk_r = ClockPort(10.ns)

    val rst_l = ResetPort()
    val rst_r = ResetPort()

    val req_l = Input(Bool())
    val req_r = Output(Bool())

    val ack_l = Output(Bool())
    val ack_r = Input(Bool())

    val data_l = Input(UInt(16.W))
    val data_r = Output(UInt(16.W))

    domain(clk_l, rst_l)(
      req_l, data_l, ack_l
    )

    domain(clk_r, rst_r)(
      req_r, data_r, ack_r
    )

  }

  Simulation(CDC(), 1.ns) { cdc =>
    
    println(cdc.domains.mkString("\n"))

    println(cdc.ports.mkString("\n"))

    fork("left") {

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

    fork("right") {

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

    cdc.clk_r.step(40)
  }

}
