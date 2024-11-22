import framework.*
import framework.given
import types.*
import Time.*

import scala.collection.mutable
import java.nio.file.Path

@main def CdcSim(): Unit = {

  class CDC extends Module("src/hdl/sv/CDC.sv") {

    val clk_l = ClockPort(6.ns)
    val clk_r = ClockPort(10.ns)

    val rst_l = ResetPort()
    val rst_r = ResetPort()

    val req_l = Input(Bool())
    val req_r = Output(Bool())

    val ack_l = Output(Bool())
    val ack_r = Input(Bool())

    val data_l = Input(UInt(128.W))
    val data_r = Output(UInt(128.W))

    domain(clk_l, rst_l)(
      req_l, data_l, ack_l
    )

    domain(clk_r, rst_r)(
      req_r, data_r, ack_r
    )

  }

  Simulation(CDC(), 1.ns) { cdc =>

    val magicNum = BigInt("deadbeefdeadbeefdeadbeefdeadbeef", 16)


    // TODO: when waiting for forks, we need to signal that the thread is sleeping to the scheduler
    
    fork {

      cdc.rst_l.assert()
      cdc.req_l.poke(false)
      cdc.data_l.poke(0)

      cdc.clk_l.step()

      cdc.rst_l.deassert()

      cdc.clk_l.step()

      cdc.req_l.poke(true)
      cdc.data_l.poke(magicNum)


      cdc.clk_l.stepUntil(cdc.ack_l.peek)
      

      cdc.req_l.poke(false)
      cdc.data_l.poke(0)


      cdc.clk_l.stepUntil(!cdc.ack_l.peek)
      

    }
    
    fork {

      cdc.rst_r.assert()
      cdc.ack_r.poke(false)

      cdc.clk_r.step()

      cdc.rst_r.deassert()

      cdc.clk_r.step()


      cdc.clk_r.stepUntil(cdc.req_r.peek)
      

      cdc.data_r.expect(magicNum)

      cdc.clk_r.step()

      cdc.ack_r.poke(true)


      cdc.clk_r.stepUntil(!cdc.req_r.peek)
      

      cdc.ack_r.poke(false)

    }

    cdc.clk_r.step(10)

  }

}
