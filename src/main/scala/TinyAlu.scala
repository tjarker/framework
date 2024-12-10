import framework.given
import framework.*
import types.*
import Time.*

import gears.async.Async
import scala.util.boundary
import TinyAlu.AluRequest
import gears.async.SyncChannel
import TinyAlu.AluTransaction

class TinyAlu extends ModuleInterface("src/hdl/sv/tinyalu.sv") {

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
    start,
    a,
    b,
    op,
    done,
    result
  )
}

object TinyAlu {

  enum Op(val enc: Int) {
    case Add extends Op(1)
    case And extends Op(2)
    case Xor extends Op(3)
    case Mul extends Op(4)

  }

  object Op {
    def fromInt(i: Int): Op = i match {
      case 1 => Add
      case 2 => And
      case 3 => Xor
      case 4 => Mul
    }
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
    AluTransaction(
      t,
      op match {
        case Op.Add => a + b
        case Op.And => a & b
        case Op.Xor => a ^ b
        case Op.Mul => a * b
      }
    )
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

  def sendRequest(t: AluRequest)(using Sim, Async.Spawn): Unit = {
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
    val prediction = TinyAlu.prediction(t)
    bfm.sendRequest(t)
    dut.clk.stepUntil(dut.done.peek)
    dut.result.expect(prediction.result)

    dut.clk.step()
  }

}

class AluDriver(dut: TinyAlu) extends Component, SimulationPhase {

  val tx = Channel[AluRequest]()
  val bfm = TinyAluBfm(dut)
  def run()(using Sim, Async.Spawn) = {

    while (true) {
      val t = tx.read().unwrap
      info(s"Sending request: $t")
      bfm.sendRequest(t)
      info(s"Waiting for done")
      bfm.waitForDone()
    }

  }
}

class AluMonitor(dut: TinyAlu) extends Component, SimulationPhase {

  val ap = Channel[AluTransaction]()
  val bfm = TinyAluBfm(dut)

  def run()(using Sim, Async.Spawn) = {

    while (true) {
      info(s"Waiting for start")
      dut.clk.stepUntil(dut.start.peek)
      info(s"Start detected")
      val t = AluRequest(
        TinyAlu.Op.fromInt(dut.op.peek.toInt),
        dut.a.peek,
        dut.b.peek
      )
      bfm.waitForDone()
      val res = dut.result.peek
      val tx = AluTransaction(t, res)
      info(s"Observed transaction: $tx")
      ap.send(tx)
      dut.clk.step()
    }

  }
}

class AluAgent(dut: TinyAlu) extends Component {

  val driver = new AluDriver(dut)
  val monitor = new AluMonitor(dut)

  val drv = driver.tx
  val ap = monitor.ap

}

class AluScoreboard(ap: Channel[AluTransaction])
    extends Component,
      SimulationPhase,
      ReportPhase {

  val txs = collection.mutable.ListBuffer[AluTransaction]()

  def run()(using Sim, Async.Spawn) = {
    while (true) {
      val tx = ap.read().unwrap
      txs += tx
      val pred = TinyAlu.prediction(tx.req)
      if (tx.result == pred.result) {
        info(s"Transaction $tx passed")
      } else {
        error(
          s"Transaction $tx failed. Expected ${pred.result}, got ${tx.result}"
        )
      }
    }

  }

  def report() = {
    info(s"Scoreboard received ${txs.size} transactions")
    info(s" - ${txs.mkString("\n - ")}")
  }

}

class AluEnv(dut: TinyAlu) extends Component {

  val agent = new AluAgent(dut)
  val scoreboard = new AluScoreboard(agent.ap)

  val drv = agent.drv
  val ap = agent.ap

}

class AluTest(dut: TinyAlu) extends Component, ResetPhase, TestPhase {

  val env = new AluEnv(dut)

  def reset()(using Sim, Async.Spawn) = {
    env.agent.driver.bfm.reset()
  }

  def test()(using Sim, Async.Spawn) = {

    util.Random.setSeed(42)

    val txs = Seq.fill(4)(AluRequest.randomize())

    txs.foreach(env.drv.send(_))

    dut.clk.step(10)
  }
}

@main def TinyAluUvm(): Unit = Simulation(TinyAlu(), 1.ps) { dut =>

  val test = Component.root(new AluTest(dut))

  Phase.run(test)

  Phase.reset(test)

  Phase.test(test)

  Phase.report(test)

}
