package apb

import gears.async.*
import framework.*


class ApbBaseSeq(using Async.Spawn, Sim) extends Sequence[ApbTransaction, ApbTransaction] {

  val delayBeforeTx = Rand.between(0, 15)

  def body(): Unit = {
    
    if delayBeforeTx > 0 then stepDomain(delayBeforeTx)

  }

  def checkResp(resp: ApbTransaction): Unit = {
    if resp.slverr then {
      error("Slave error detected")
    }
  }

  def yieldApbTx(tx: ApbTransaction): Unit = {
    checkResp(yieldTx(tx))
  }

}


class ApbSingleSeq(using Async.Spawn, Sim) extends ApbBaseSeq {

  override def body(): Unit = {
    super.body()

    yieldApbTx(new ApbTransaction)
  }

}

class ApbSingleZdSeq(using Async.Spawn, Sim) extends ApbSingleSeq {

  override val delayBeforeTx: Int = 0

  override def body(): Unit = {
    super.body()

    val tx = new ApbTransaction
    tx.noWaitLen = true

    yieldApbTx(tx)
  }

}

class ApbRandomSeq(len: Int)(using Async.Spawn, Sim) extends ApbBaseSeq {

  override def body(): Unit = {
    super.body()

    for i <- 0 until len do {
      val tx = new ApbTransaction
      warning(s"Generated transaction $i: $tx")
      yieldApbTx(tx)
    }
  }

}