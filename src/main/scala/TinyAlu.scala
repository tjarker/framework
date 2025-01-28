import framework.given
import framework.*
import types.*
import Time.*

import scala.util.boundary
import TinyAlu.AluRequest
import TinyAlu.AluResult

import collection.mutable

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
  class AluRequest(val op: Op, val a: BigInt, val b: BigInt)
      extends Transaction {
    def randomize(): AluRequest = {
      val a = BigInt(8, scala.util.Random)
      val b = BigInt(8, scala.util.Random)
      val op = Op.values.oneOf
      AluRequest(op, a, b)
    }

    override def toString(): String = s"AluRequest($op, $a, $b)"
  }

  class AluResult(val req: AluRequest, val result: BigInt) extends Transaction {
    override def toString(): String = s"AluResult($req, $result)"
  }

  def prediction(t: AluRequest): AluResult = {
    val a = t.a
    val b = t.b
    val op = t.op
    AluResult(
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

class AluDriver(using Hierarchy)
    extends Driver[AluRequest, AluResult],
      SimulationPhase {

  val dut = param[TinyAlu]("dut")
  val bfm = TinyAluBfm(dut)
  def sim()(using Sim, Async.Spawn) = foreachTx { t =>
    info(s"Sending request: $t")
    bfm.sendRequest(t)
    info(s"Waiting for done")
    bfm.waitForDone()
    respond(AluResult(t, bfm.getResult()))
  }

}

class AluMonitor(using Hierarchy) extends Monitor[AluResult], SimulationPhase {

  val dut = param[TinyAlu]("dut")
  val bfm = TinyAluBfm(dut)

  def sim()(using Sim, Async.Spawn) = {

    while (true) {
      info(s"Waiting for start")
      bfm.waitForStart()
      info(s"Start detected")
      val t = AluRequest(
        TinyAlu.Op.fromInt(dut.op.peek.toInt),
        dut.a.peek,
        dut.b.peek
      )
      bfm.waitForDone()
      val res = bfm.getResult()
      val tx = AluResult(t, res)
      info(s"Observed transaction: $tx")
      publish(tx)
      dut.clk.step()
    }

  }
}

class AluAgent(using Hierarchy) extends Component {

  val driver = Comp.create[AluDriver]
  val seq = Comp.create[Sequencer[AluRequest, AluResult]]
  val monitor = Comp.create[AluMonitor]

  driver.port.connect(seq.port)

}

class AluScoreboard(using Hierarchy)
    extends AnalysisComponent[AluResult],
      ReportPhase {

  val passing = mutable.ListBuffer[AluResult]()
  val failing = mutable.ListBuffer[AluResult]()

  def sim()(using Sim, Async.Spawn) = foreachTx { tx =>
    val pred = TinyAlu.prediction(tx.req)
    if (tx.result == pred.result) {
      info(s"Transaction $tx passed")
      passing += tx
    } else {
      error(
        s"Transaction $tx failed. Expected ${pred.result}, got ${tx.result}"
      )
      failing += tx
    }
  }

  def report() = {
    info(
      s"Passing transactions: ${passing.size}" + "\n" + passing.mkString("\n")
    )
    info(
      s"Failing transactions: ${failing.size}" + "\n" + failing.map(tx => s"$tx (expected ${TinyAlu.prediction(tx.req)})").mkString("\n")
    )
  }

}

class AluCoverage(using Hierarchy) extends AnalysisComponent[AluResult] {

  val ops = mutable.Map[TinyAlu.Op, Int]().withDefaultValue(0)

  def sim()(using Sim, Async.Spawn) = foreachTx { tx =>
    ops(tx.req.op) += 1
  }

  def report() = {
    val str = ops.map { case (op, cnt) => s"$op: $cnt hits" }.mkString("\n")
    info(s"Coverage:\n$str")
  }

}

class AluEnv(using Hierarchy) extends Component {

  val agent = Comp.create[AluAgent]
  val scoreboard = Comp.create[AluScoreboard]
  val coverage = Comp.create[AluCoverage]

  agent.monitor.addListeners(scoreboard, coverage)
}

class RandomSeq(using Sim, Async.Spawn)
    extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    for (op <- TinyAlu.Op.values) {
      val a = BigInt(8, scala.util.Random)
      val b = BigInt(8, scala.util.Random)
      yieldTx(AluRequest(op, a, b))
    }
  }
}

class MaxSeq(using Sim, Async.Spawn) extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    for (op <- TinyAlu.Op.values) {
      val a = 0xff
      val b = 0xff
      yieldTx(AluRequest(op, a, b))
    }
  }
}

class ManualSeq(op: TinyAlu.Op, a: BigInt, b: BigInt)(using Sim, Async.Spawn)
    extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    yieldTx(AluRequest(op, a, b))
  }
}

class FibonacciSeq(using Sim, Async.Spawn)
    extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    var a = 0
    var b = 1
    for (i <- 0 until 10) {
      yieldTx(AluRequest(TinyAlu.Op.Add, a, b))
      val c = a + b
      a = b
      b = c
    }
  }
}

class TestAllSeq(using Sim, Async.Spawn)
    extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    yieldSeq(RandomSeq())
    yieldSeq(MaxSeq())
    yieldSeq(FibonacciSeq())
  }
}

class TestAllSeqParallel(using Sim, Async.Spawn)
    extends Sequence[AluRequest, AluResult] {

  protected def body(): Unit = {
    fork {
      yieldSeq(RandomSeq())
    }.fork {
      yieldSeq(MaxSeq())
    }.fork {
      yieldSeq(FibonacciSeq())
    }.join()
  }
}

class AluTest(dut: TinyAlu)(using Hierarchy) extends Test, ResetPhase {

  Comp.set("dut", dut)
  val bfm = TinyAluBfm(dut)
  val env = Comp.create[AluEnv]

  def reset()(using Sim, Async.Spawn) = {
    bfm.reset()
  }

  def test()(using Sim, Async.Spawn) = {
    util.Random.setSeed(42)
    val seq = TestAllSeq()
    env.agent.seq.play(seq)
    seq.waitUntilDone()
  }
}

@main def TinyAluUvm(): Unit =
  Test.run(new TinyAlu, 1.ps, Some("alu.vcd"))(new AluTest(_))
