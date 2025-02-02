package apb

import gears.async.*
import framework.*


class ApbBaseSeq(using Hierarchy) extends Sequence[ApbTransaction, ApbTransaction] {

  val delayBeforeTx = Rand.between(0, 15)

  def body()(using Sim, Async.Spawn): Unit = {
    
    if delayBeforeTx > 0 then stepClockDomain(delayBeforeTx)

  }

  def checkResp(resp: ApbTransaction)(using Sim, Async): Unit = {
    if resp.slverr then {
      error("Slave error detected")
    }
  }

  def yieldApbTx(tx: ApbTransaction)(using Sim, Async): Unit = {
    checkResp(yieldTx(tx))
  }

}


class ApbSingleSeq(using Hierarchy) extends ApbBaseSeq {

  override def body()(using Sim, Async.Spawn): Unit = {
    super.body()

    yieldApbTx(new ApbTransaction)
  }

}

class ApbSingleZdSeq(using Hierarchy) extends ApbSingleSeq {

  override val delayBeforeTx: Int = 0

  override def body()(using Sim, Async.Spawn): Unit = {
    super.body()

    val tx = new ApbTransaction
    tx.noWaitLen = true

    yieldApbTx(tx)
  }

}

class ApbRandomSeq(using Hierarchy) extends ApbBaseSeq {

  val len = Config.getOrElse("len", 10)

  override def body()(using Sim, Async.Spawn): Unit = {
    super.body()

    for i <- 0 until len do {
      val tx = new ApbTransaction
      warning(s"Generated transaction $i: $tx")
      yieldApbTx(tx)
    }
  }

}