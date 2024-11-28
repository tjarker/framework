
import framework.*
import types.*
import Time.*
import framework.given

import gears.async.Async
import scala.util.boundary
import TinyAlu.AluRequest
import gears.async.SyncChannel
import TinyAlu.AluTransaction

class TinyAlu extends Module("src/hdl/sv/tinyalu.sv") {

  override val name = "tinyalu"

  val clk = ClockPort(2.ps)
  val reset_n = ResetPort()

  val start = Input(Bool())

  val a = Input("A", UInt(8.W))
  val b = Input("B", UInt(8.W))
  val op = Input(UInt(3.W))

  val done = Output(Bool())
  val result = Output(UInt(8.W))

  domain(clk, reset_n)(
    start, a, b, op, done, result
  )
}

object TinyAlu {

  enum Op(val enc: Int) {
    case Add extends Op(1)
    case And extends Op(2)
    case Xor extends Op(3)
    case Mul extends Op(4)
  }

  object AluRequest {
    def randomize(): AluRequest = {
      val a = BigInt(8, scala.util.Random)
      val b = BigInt(8, scala.util.Random)
      val op = Op.values.oneOf
      AluRequest(op, a, b)
    }
  }
  case class AluRequest(op: Op, a: BigInt, b: BigInt) {
    def randomize(): AluRequest = {
      val a = BigInt(8, scala.util.Random)
      val b = BigInt(8, scala.util.Random)
      val op = Op.values.oneOf
      AluRequest(op, a, b)
    }
  }

  case class AluTransaction(req: AluRequest, result: BigInt)

  def prediction(t: AluRequest): AluTransaction = {
    import t.{a, b, op}
    AluTransaction(t, op match {
      case Op.Add => a + b
      case Op.And => a & b
      case Op.Xor => a ^ b
      case Op.Mul => a * b
    })
  }


}

class TinyAluBfm(dut: TinyAlu) {

  def reset()(using Sim, Async) = {
    dut.reset_n.deassert()
    dut.start.poke(false)
    dut.clk.step()
    dut.reset_n.assert()
    dut.clk.step()
  }

  def sendRequest(t: AluRequest)(using Sim, Async.Spawn): Unit = fork {
    import t.{a, b, op}
    dut.a.poke(a)
    dut.b.poke(b)
    dut.op.poke(op.enc)
    dut.start.poke(true)
    dut.clk.step()
    dut.start.poke(false)
  }

  def waitForDone()(using Sim, Async) = {
    dut.clk.stepUntil(dut.done.peek)
  }

  def waitForStart()(using Sim, Async) = {
    dut.clk.stepUntil(dut.start.peek)
  }

  def getResult()(using Sim, Async): BigInt = {
    dut.result.peek
  }

}

@main def TinyAluTest(): Unit = Simulation(TinyAlu(), 1.ps) { dut =>

  val bfm = TinyAluBfm(dut)

  bfm.reset()

  for {
    t <- Seq.fill(4)(AluRequest.randomize())
  } {
    val a = BigInt(8, scala.util.Random)
    val b = BigInt(8, scala.util.Random)
    val prediction = TinyAlu.prediction(t)
    bfm.sendRequest(t)
    dut.clk.stepUntil(dut.done.peek)
    dut.result.expect(prediction.result)

    dut.clk.step()
  }

}




class AluDriver(dut: TinyAlu) extends Component, RunPhase {

  val tx = SyncChannel[AluRequest]()
  val bfm = TinyAluBfm(dut)
  def run()(using Sim, Async.Spawn) = {
    while (true) {
      tx.read() match {
        case Right(t) => 
          info(s"Sending request: $t")
          bfm.sendRequest(t)
        case _ => 
      }
      bfm.waitForDone()
    }
  }

}

class AluMonitor(dut: TinyAlu) extends Component, RunPhase {

  val ap = SyncChannel[AluTransaction]()
  val bfm = TinyAluBfm(dut)

  def run()(using Sim, Async.Spawn) = {
    while (true) {
      bfm.waitForStart()
      val t = AluRequest(TinyAlu.Op.fromOrdinal(dut.op.peek.toInt), dut.a.peek, dut.b.peek)
      bfm.waitForDone()
      val res = dut.result.peek
      val tx =AluTransaction(t, res)
      info(s"Observed transaction: $tx")
      ap.send(tx)
    }
  }
}

class AluAgent(dut: TinyAlu) extends Component {

  val driver = new AluDriver(dut)
  val monitor = new AluMonitor(dut)

  val drv = driver.tx
  val ap = monitor.ap

}

class AluScoreboard(ap: SyncChannel[AluTransaction]) extends Component, RunPhase {

  def run()(using Sim, Async.Spawn) = {
    while (true) {
      val tx = ap.read().right.get
      val pred = TinyAlu.prediction(tx.req)
      if (tx.result == pred.result) {
        info(s"Transaction $tx passed")
      } else {
        error(s"Transaction $tx failed. Expected ${pred.result}, got ${tx.result}")
      }
    }
  }

}

class AluEnv(dut: TinyAlu) extends Component {

  val agent = new AluAgent(dut)
  val scoreboard = new AluScoreboard(agent.ap)

  val drv = agent.drv
  val ap = agent.ap

}

class AluTest(dut: TinyAlu) extends Component, RunPhase {

  val env = new AluEnv(dut)

  def run()(using Sim, Async.Spawn) = {
    val txs = Seq.fill(4)(AluRequest.randomize())

    txs.foreach(env.drv.send(_))
  }
}


@main def TinyAluUvm(): Unit = Simulation(TinyAlu(), 1.ps) { dut =>

  val env = Component.root(new AluEnv(dut))

  Phase.run(env).await


  Phase.report(env)

}