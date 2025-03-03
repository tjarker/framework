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

  enum Op(val enc: Int, val sym: String) {
    case Add extends Op(1, "+")
    case And extends Op(2, "&")
    case Xor extends Op(3, "^")
    case Mul extends Op(4, "*")

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
    inline def random(): AluRequest = {
      val a = Rand.uint(8.W)
      val b = Rand.uint(8.W)
      val op = Rand.oneof(Op.values)
      AluRequest(op, a, b)
    }
  }
  class AluRequest(val op: Op, val a: BigInt, val b: BigInt)
      extends Transaction {
    def randomize(): AluRequest = {
      val a = Rand.uint(8.W)
      val b = Rand.uint(8.W)
      val op = Rand.oneof(Op.values)
      AluRequest(op, a, b)
    }

    override def toString(): String = s"AluRequest($a ${op.sym} $b)"
  }

  class AluResult(val req: AluRequest, val result: BigInt) extends Transaction {
    override def toString(): String = s"AluResult($req = $result)"
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
    dut.clk.stepUntil(dut.start.peekMonitor)
  }

  def getResult()(using Sim, Async): BigInt = {
    dut.result.peek
  }

  def expectResult(t: BigInt)(using Sim, Async) = {
    dut.result.expect(t)
  }

  def observeTransaction()(using Sim, Async): AluResult = {
    dut.clk.stepUntil(dut.start.peekMonitor)
    val t = AluRequest(
      TinyAlu.Op.fromInt(dut.op.peekMonitor),
      dut.a.peekMonitor,
      dut.b.peekMonitor
    )
    dut.clk.stepUntil(dut.done.peek)
    val res: BigInt = dut.result.peek
    AluResult(t, res)
  }

}

class AluDriver(using Hierarchy)
    extends Driver[AluRequest, AluResult],
      SimulationPhase, ReportPhase {

  val dut = param[TinyAlu]
  val bfm = TinyAluBfm(dut)
  def sim()(using Sim, Async.Spawn) = foreachTx { t =>
    //info(s"Sending request: $t")
    bfm.sendRequest(t)
    bfm.waitForDone()
    //info(s"Got result: ${bfm.getResult()}")
    respond(AluResult(t, bfm.getResult()))
  }

  def report() = {
    info(s"Driven ${numOfDrivenTxs} transactions")
  }

}

class AluMonitor(using Hierarchy) extends Monitor[AluResult], SimulationPhase {

  val dut = param[TinyAlu]
  val bfm = TinyAluBfm(dut)

  def sim()(using Sim, Async.Spawn) = forever {
    val tx = bfm.observeTransaction()
    publish(tx)
    dut.clk.step()
  }

}

class AluScoreboard(using Hierarchy)
    extends AnalysisComponent[AluResult],
      ReportPhase {

  val passing = mutable.ListBuffer[AluResult]()
  val failing = mutable.ListBuffer[AluResult]()

  def sim()(using Sim, Async.Spawn) = foreachTx { tx =>
    val pred = TinyAlu.prediction(tx.req)
    if (tx.result == pred.result) {
      passing += tx
    } else {
      failing += tx
    }
  }

  def report() = {
    passing.foreach { tx =>
      //info(s"PASSED: $tx")
    }
    failing.foreach { tx =>
      if (Config.get[AluTestConfig].CheckErrors) {
        error(s"FAILED: $tx expected ${TinyAlu.prediction(tx.req)}")
      } else {
        //info(s"FAILED: $tx expected ${TinyAlu.prediction(tx.req)}")
      }
    }
  }

}

class AluCoverage(using Hierarchy) extends AnalysisComponent[AluResult] {

  val ops = mutable.Map[TinyAlu.Op, Int](TinyAlu.Op.values.map(_ -> 0)*)

  def sim()(using Sim, Async.Spawn) = foreachTx { tx =>
    ops(tx.req.op) += 1
  }

  def report() = {
    if (Config.get[AluTestConfig].CoverageErrors) {
      ops.foreach { case (op, cnt) =>
        if (cnt == 0) error(s"Operation $op not covered")
      }
      if (!ops.exists(_._2 == 0)) info("All operations covered")
    }
  }

}

class AluEnv(using Hierarchy) extends Component {

  val driver = Factory.create[AluDriver]
  val seq = Factory.create[Sequencer[AluRequest, AluResult]]

  val monitor = Factory.create[AluMonitor]
  val scoreboard = Factory.create[AluScoreboard]
  val coverage = Factory.create[AluCoverage]

  driver.port.connect(seq.port)
  monitor.addListeners(scoreboard, coverage)
}

abstract class AluSeq(using Hierarchy) extends Sequence[AluRequest, AluResult] 

class RandomSeq(using Hierarchy) extends AluSeq {

  protected def body()(using Sim, Async.Spawn) = {
    for (op <- TinyAlu.Op.values) {
      val a = Rand.uint(8.W)
      val b = Rand.uint(8.W)
      yieldTx(AluRequest(op, a, b))
    }
  }
}

class MaxSeq(using Hierarchy) extends AluSeq {

  protected def body()(using Sim, Async.Spawn): Unit = {
    for (op <- TinyAlu.Op.values) {
      yieldTx(AluRequest(op, 0xff, 0xff))
    }
  }
}

class ManualSeq(op: TinyAlu.Op, a: BigInt, b: BigInt)(using Hierarchy)
    extends AluSeq {

  protected def body()(using Sim, Async.Spawn): Unit = {
    yieldTx(AluRequest(op, a, b))
  }
}

class FibonacciSeq(using Hierarchy) extends AluSeq {

  protected def body()(using Sim, Async.Spawn): Unit = {
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

class TestAllSeq(using Hierarchy) extends AluSeq {

  protected def body()(using Sim, Async.Spawn): Unit = {
    for (i <- 0 until 1000) {
      info(s"Running iteration $i")
      val rand = Factory.create[RandomSeq]
      val max = Factory.create[MaxSeq]
      rand.start()
      max.start()
      yieldSeq(rand)
      yieldSeq(max)
    }
  }
}

class TestAllSeqParallel(using Hierarchy)
    extends AluSeq {

  protected def body()(using Sim, Async.Spawn): Unit = {
    val rand = Factory.create[RandomSeq]
    val max = Factory.create[MaxSeq]
    val fib = Factory.create[FibonacciSeq]
    rand.start()
    max.start()
    fib.start()

    val mix = SequenceComposition.Mix(rand, max, fib)
    mix.start()
    yieldSeq(mix)
  }
}

class AluTestConfig {
  val CoverageErrors = true
  val CheckErrors = true
}

class AluTestConfigNoErr extends AluTestConfig {
  override val CoverageErrors = false
  override val CheckErrors = false
}

class AluTest(dut: TinyAlu)(using Hierarchy) extends Test, ResetPhase {

  Config.set("dut", dut)

  val bfm = TinyAluBfm(dut)
  val env = Factory.builder
    .withParams("hello" -> "world")
    .withConfigOverride[AluTestConfig, AluTestConfigNoErr]
    .withTypeOverride[AluDriver, AluDriver]
    .create[AluEnv]

  def reset()(using Sim, Async.Spawn) = {
    bfm.reset()
  }

  def test()(using Sim, Async.Spawn) = {
    util.Random.setSeed(42)
    val seq = Factory.create[AluSeq]
    seq.start()
    env.seq.play(seq)
    seq.waitUntilDone()
  }
}

class FibonacciTest(dut: TinyAlu)(using Hierarchy) extends AluTest(dut) {

  override def overrides(using Hierarchy): Unit = Factory.overrideType[AluSeq, FibonacciSeq]

}

class ParallelTest(dut: TinyAlu)(using Hierarchy) extends AluTest(dut) {

  override def overrides(using Hierarchy): Unit = Factory.overrideType[AluSeq, TestAllSeqParallel]

}

@main def AluTestRandom(): Unit =
  Test.run(new TinyAlu, 1.ps, Some("alu_rand.vcd"))(new AluTest(_))

@main def AluTestFibonacci(): Unit =
  Test.run(new TinyAlu, 1.ps, Some("alu_fib.vcd"))(new FibonacciTest(_))

@main def AluTestParallel(): Unit =
  Test.run(new TinyAlu, 1.ps, Some("alu_parallel.vcd"))(new ParallelTest(_))

object TinyAluTests extends TestSuite {

  testModule(new TinyAlu) {

    test("simple add")
      .withDump("addtest.vcd")
      .withResolution(1.ps)
      .in { dut =>
        val bfm = TinyAluBfm(dut)
        bfm.reset()
        bfm.sendRequest(AluRequest(TinyAlu.Op.Add, 1, 2))
        bfm.waitForDone()
        bfm.expectResult(3)
      }

    test("random")
      .withTest(new AluTest(_))

    test("max")
      .withTest {
        new AluTest(_) {
          override def overrides(using Hierarchy): Unit = Factory.overrideType[AluSeq, MaxSeq]
        }
      }

    test("fibonacci")
      .withDump("fibtest.vcd")
      .withTest(new FibonacciTest(_))

    test("parallel")
      .withTest(new ParallelTest(_))

    test("all")
      .withDump("alltest.vcd")
      .withTest {
        new AluTest(_) {
          override def overrides(using Hierarchy): Unit = Factory.overrideType[AluSeq, TestAllSeq]
        }
      }
  }

}
