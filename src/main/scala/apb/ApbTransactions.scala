package apb

import framework.*
import framework.types.*

enum OpType(val value: Int) {
  case Read extends OpType(0)
  case Write extends OpType(1)
}

enum NormalAccess(val value: Int) {
  case Normal extends NormalAccess(0)
  case Privileged extends NormalAccess(1)
}

enum SecureAccess(val value: Int) {
  case Secure extends SecureAccess(0)
  case NonSecure extends SecureAccess(1)
}

enum DataAccess(val value: Int) {
  case Data extends DataAccess(0)
  case Instruction extends DataAccess(1)
}

class ApbTransaction extends Transaction {

  var noWaitLen = Rand.bool()
  var maxWaitLen = 15

  var op = Rand.oneof(OpType.values)
  var addr = if(op == OpType.Read) Rand.oneof(Seq(0,4)) else 0
  var data = Rand.uint(32.W)
  var strb = Rand.uint(4.W)
  var slverr = Rand.bool()


  var normAcc = Rand.oneof(NormalAccess.values)
  var secAcc = Rand.oneof(SecureAccess.values)
  var dataAcc = Rand.oneof(DataAccess.values)

  var waitLen = if noWaitLen then 0 else Rand.between(0, maxWaitLen)


  override def toString(): String = {
    s"ApbTransaction(op=$op, addr=$addr, data=$data, strb=$strb, slverr=$slverr, normAcc=$normAcc, secAcc=$secAcc, dataAcc=$dataAcc, waitLen=$waitLen)"
  } 

}