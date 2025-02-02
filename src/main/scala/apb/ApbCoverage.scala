package apb

import framework.*

import scala.collection.mutable

class ApbCoverage(using Hierarchy) extends AnalysisComponent[ApbTransaction] {

  val op = mutable.Map[OpType, Int](OpType.values.map(_ -> 0)*)
  val addr = mutable.Map[Int, Int]().withDefaultValue(0)
  val err = mutable.Map[Boolean, Int](true -> 0, false -> 0)

  val opErr = mutable.Map[(OpType, Boolean), Int](OpType.values.flatMap { op =>
    Seq((op, true) -> 0, (op, false) -> 0)
  }*).withDefaultValue(0)

  val data = mutable.Map[BigInt, Int]().withDefaultValue(0)

  val delayBeforeTx = mutable.Map[Int, Int]().withDefaultValue(0)

  def sim()(using Sim, Async.Spawn): Unit = {
    foreachTx { tx =>
      op(tx.op) += 1
      addr(tx.addr) += 1
      err(tx.slverr) += 1
      data(tx.data) += 1
      opErr(tx.op -> tx.slverr) += 1
      delayBeforeTx(tx.waitLen) += 1
    }
  }

  def report(): Unit = {
    val str = s"""Coverage report:
      |Op coverage:
      |${op.map { case (k, v) => s"$k: $v" }.mkString("\n")}
      |
      |Addr coverage:
      |${addr.map { case (k, v) => s"$k: $v" }.mkString("\n")}
      |
      |Error coverage:
      |${err.map { case (k, v) => s"$k: $v" }.mkString("\n")}
      |
      |Op error cross coverage:
      |${opErr.map { case ((op, err), v) => s"$op, $err: $v" }.mkString("\n")}
      |
      |Data coverage:
      |${data.map { case (k, v) => s"${k.toString(16)}: $v" }.mkString("\n")}
      |
      |Delay before transaction coverage:
      |${delayBeforeTx.map { case (k, v) => s"$k: $v" }.mkString("\n")}
      |""".stripMargin
    info(str)
  }

}
