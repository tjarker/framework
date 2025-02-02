package apb

import framework.*
import framework.types.*

class ApbMonitor(using Hierarchy) extends Monitor[ApbTransaction] {

  val bfm = Config.get[ApbBfm]("bfm")


  def sim()(using Sim, Async.Spawn) = forever {
      val tx = new ApbTransaction

      var waitStates = 0

      while (!bfm.en.peek || !bfm.ready.peek) {
        if (bfm.sel.peek && !bfm.ready.peek) {
          waitStates += 1
        }
        bfm.clk.step()
      }

      tx.waitLen = waitStates
      tx.addr = bfm.addr.peek.toInt
      tx.slverr = bfm.slverr.peek

      if (bfm.wr.peek) {
        tx.op = OpType.Write
        tx.data = bfm.wdata.peek.toInt
      } else {
        tx.op = OpType.Read
        tx.data = bfm.rdata.peek.toInt
      }

      bfm.clk.step()

      publish(tx)
    }


}
