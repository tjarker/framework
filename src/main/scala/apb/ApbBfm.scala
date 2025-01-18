package apb

import framework.*
import framework.types.*

import gears.async.*

class ApbBfm(
    val clk: ClockPort,
    val rst: ResetPort,
    val addr: Input[UInt],
    val en: Input[Bool],
    val sel: Input[Bool],
    val wdata: Input[UInt],
    val wr: Input[Bool],
    val rdata: Output[UInt],
    val ready: Output[Bool],
    val slverr: Output[Bool]
) {

  def reset()(using Sim, Async): Unit = {
    this.en.poke(true)
    this.sel.poke(false)

    this.clk.step()
    this.rst.assert()
    this.clk.step(5)
  }

  def write(addr: BigInt, data: BigInt)(using Sim, Async): Option[Unit] = {

    this.addr.poke(addr)
    this.en.poke(false)
    this.sel.poke(true)
    this.wdata.poke(data)
    this.wr.poke(true)

    this.clk.step()
    this.en.poke(true)
    this.clk.stepUntil(this.ready.peek)

    val res =
      if this.slverr.peek then None
      else Some(())


    res
  }

  def read(addr: BigInt)(using Sim, Async): Option[BigInt] = {

    this.addr.poke(addr)
    this.en.poke(false)
    this.sel.poke(true)
    this.wr.poke(false)

    this.clk.step()
    this.en.poke(true)

    this.clk.stepUntil(this.ready.peek)

    val data = this.rdata.peek
    val res = if this.slverr.peek then None else Some(data)

    res

  }

}
