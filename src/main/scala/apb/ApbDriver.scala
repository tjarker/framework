package apb

import framework.*
import gears.async.Async.Spawn
import gears.async.Async
import framework.types.*
import framework.types.stepUntil

abstract class ApbBaseDriver(using Hierarchy) extends Driver[ApbTransaction, ApbTransaction] {


  val bfm = param[ApbBfm]("bfm")

  override def sim()(using Sim, Spawn): Unit = {

    driverLoop()

  }

  def drivePins(tx: ApbTransaction)(using Sim, Async): ApbTransaction


  def driverLoop()(using Sim, Async): Unit = while(true) {

    info("Waiting for next transaction")

    val tx = next()

    info(s"Got transaction $tx")

    info("Driving pins")
    val resp = drivePins(tx)

    info(s"Responding with $resp")
    respond(resp)
  }

}

class ApbProducerDriver(using Hierarchy) extends ApbBaseDriver {

  override def drivePins(tx: ApbTransaction)(using Sim, Async): ApbTransaction = {

    info("Driving pins for producer")


    tx.op match {
      case OpType.Write => {
        bfm.addr.poke(tx.addr)
        bfm.en.poke(false)
        bfm.sel.poke(true)
        bfm.wdata.poke(tx.data)
        bfm.wr.poke(true)
      }
      case OpType.Read => {
        bfm.addr.poke(tx.addr)
        bfm.en.poke(false)
        bfm.sel.poke(true)
        bfm.wr.poke(false)
      }
    }

    bfm.clk.step()
    bfm.en.poke(true)

    bfm.clk.stepUntil(bfm.ready.peek)

    val err = bfm.slverr.peek
    val rdata = bfm.rdata.peek

    info(s"Got response: $rdata, error: $err")


    val resp = tx.copy()

    resp.slverr = err
    tx.op match {
      case OpType.Read => resp.data = rdata
      case _ => ()
    }

    resp
  }

}