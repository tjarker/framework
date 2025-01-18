package apb

import gears.async.*
import framework.*


class ApbBaseSeq(using Async, Sim) extends Sequence[ApbTransaction] {

  val delayBeforeTx = Rand.between(0, 15)

  def body: Unit = {
    
    if delayBeforeTx > 0 then stepDomain(delayBeforeTx)

  }

}


class ApbSingleSeq(using Async, Sim) extends ApbBaseSeq {

  override def body: Unit = {
    super.body

    doYield(new ApbTransaction)
  }

}

class ApbSingleZdSeq(using Async, Sim) extends ApbSingleSeq {

  override val delayBeforeTx: Int = 0

  override def body: Unit = {
    super.body

    val tx = new ApbTransaction
    tx.noWaitLen = true

    doYield(tx)
  }

}

class ApbRandomSeq(len: Int)(using Async, Sim) extends ApbBaseSeq {

  override def body: Unit = {
    super.body

    for _ <- 0 until len do {
      doYield(new ApbTransaction)
    }
  }

}