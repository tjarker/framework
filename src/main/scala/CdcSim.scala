import com.sun.jna._

object CdcSim extends App {

  val lib =
    Native.load("./cdc_hs/build/libcdc.so", classOf[SimpleModelInterface])

  // inputs

  val rst_l = 2
  val rst_r = 3
  val req_l = 4
  val ack_r = 5
  val data_l = 6

  // outputs
  val req_r = 0
  val ack_l = 1
  val data_r = 2

  val ctx = lib.createSimContext("CDC", "cdc.vcd", "1ns")

  class Clock(val id: Long, val halfPeriod: Long) {
    var nextTick = halfPeriod
    var value = false
  }

  val clk_l = Clock(0, 4)
  val clk_r = Clock(1, 10)

  var time = 0L

  def tick() = lib.tick(ctx)

  def nextClockToTick: Clock = {
    List(clk_l, clk_r).minBy(_.nextTick)
  }

  def doClockTick(clk: Clock) = {
    clk.value = !clk.value
    lib.setInput(ctx, clk.id, if (clk.value) 1 else 0)
    clk.nextTick += clk.halfPeriod
  }

}
