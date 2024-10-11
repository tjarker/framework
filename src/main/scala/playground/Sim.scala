
import com.sun.jna.{InvocationMapper, Library, Native, NativeLibrary, Pointer, Callback}

import java.lang.reflect.{InvocationHandler, Method}


trait SimpleModelInterface extends Library {
  def createSimContext(name: String, waveFile: String, timeResolution: String): Pointer
  def destroySimContext(ctx: Pointer): Unit
  def setInput(ctx: Pointer, id: Long, value: Long): Unit
  def getOutput(ctx: Pointer, id: Long): Long
  def tick(ctx: Pointer): Unit
}

trait ModelInterface extends SimpleModelInterface {

  trait bool_cb extends Callback {
    def invoke(): Boolean
  }
  def eval(cb: bool_cb): Unit

  trait event_cb extends Callback {
    def invoke(outputs: Pointer): Boolean
  }
  def tickUntil(ctx: Pointer, cb: event_cb): Long
}


object Sim extends App {


  val start = System.nanoTime()

  val lib = Native.load("./gcd/build/libmodel.so", classOf[ModelInterface])


  val ctx = lib.createSimContext("GCD", "gcd.vcd", "1ns")

  lib.tick(ctx)
  lib.tick(ctx)

  lib.setInput(ctx, 0, 0x4444)
  lib.setInput(ctx, 1, 0x700C)
  lib.setInput(ctx, 2, 1)

  lib.tick(ctx)

  lib.setInput(ctx, 2, 0)

  lib.tick(ctx)

  // val ctx2 = lib.createSimContext("GCD", "gcd2.vcd", "10ns")

  // lib.setInput(ctx2, 0, 0x1343)
  // lib.setInput(ctx2, 1, 0x43211)
  // lib.setInput(ctx2, 2, 1)

  // lib.tick(ctx2)

  // lib.setInput(ctx2, 2, 0)

  // lib.tick(ctx2)

  // lib.tickUntil(ctx2, new lib.event_cb {
  //   def invoke(outputs: Pointer): Unit = {
  //     outputs.getLongArray(0, 2).apply(0) != 0
  //   }
  // })

  // println(s"Result for model 2: ${lib.getOutput(ctx2, 1)}")

  // lib.destroySimContext(ctx2)


  lib.tickUntil(ctx, new lib.event_cb {
    def invoke(outputs: Pointer): Boolean = {
      outputs.getLongArray(0, 2).apply(0) != 0
    }
  })
  // while (lib.getOutput(ctx, 0) == 0) {
  //   lib.tick(ctx)
  // }

  println(s"Result for model 1: ${lib.getOutput(ctx, 1)}")

  lib.destroySimContext(ctx)

  val end = System.nanoTime()
  val duration = (end - start) / 1e6 // Convert to milliseconds
  println(s"Execution time: $duration ms")
}


/*

trait ModelInterface extends Library {
  def sim_init(): Unit
  def sim_close(): Unit
  def set_tick_get(in_ids: Array[Long], in_vals: Array[Long], in_size: Int, out_vals: Array[Long], ticks: Long): Unit
  def set_tick_get_until_equal(in_ids: Array[Long], in_vals: Array[Long], in_size: Int, out_vals: Array[Long], port_id: Long, value: Long): Long

  def unload(): Unit
}


class ModelState(nInPorts: Int, nOutPorts: Int) {
  val inPorts = Array.fill[Long](nInPorts)(0)
  val inChanged = Array.fill[Boolean](nInPorts)(false)
  val outPorts = Array.fill[Long](nOutPorts)(0)

  def setInPort(id: Long, value: Long): Unit = {
    inPorts(id.toInt) = value
    inChanged(id.toInt) = true
  }

  def getInPort(id: Long): Long = inPorts(id.toInt)

  def getOutPort(id: Long): Long = outPorts(id.toInt)

  def getInChanges(): (Array[Long], Array[Long]) = {
    val changes = inPorts.zipWithIndex.filter((v,i) => inChanged(i)).map((v,i) => (i.toLong,v)).unzip
    for (i <- changes.*1) inChanged(i.toInt) = false
    changes
  }

}

trait Port {
  
  def peek(): Long
}

case class Output(id: Long)(using state: ModelState) extends Port {
  def peek(): Long = state.getOutPort(id)
}

case class Input(id: Long)(using state: ModelState) extends Port {
  def peek(): Long = state.getInPort(id)
  def poke(value: Long): Unit = state.setInPort(id, value)
}

case class Clock()(using ticker: (Either[Int, (Output, Long)] => Unit)) {
  def tick(n: Int = 1): Unit = ticker(Left(n))
  def tickUntil(out: Output, value: Long): Unit = ticker(Right((out, value)))
}

enum Event(id: Long) {
  case Posedge(p: Output) extends Event(0)
  case Negedge(p: Output) extends Event(1)
  case Change(p: Output) extends Event(2)

  case EqualConst(p: Output, v: Long) extends Event(3)
  case NotEqualConst(p: Output, v: Long) extends Event(4)
  case Equal(p1: Output, p2: Output) extends Event(5)
  case NotEqual(p1: Output, p2: Output) extends Event(6)

  case ClockTicks(c: Clock, n: Int) extends Event(7)

  case TimeDelta(d: Long) extends Event(8)
  case TimeAbs(t: Long) extends Event(9)

}

abstract class Model(nInPorts: Int, nOutPorts: Int) {

  val opts = new java.util.HashMap[String, Object];

  opts.put(
    Library.OPTION_INVOCATION_MAPPER, new InvocationMapper {
      override def getInvocationHandler(lib: NativeLibrary, m: Method): InvocationHandler = {
        if (m.getName == "unload") {
          return (proxy, method, args) => {
            lib.dispose()
            println("Library closed")
            null
          }
        }
        return null
      }
    }
  )

  given model: ModelInterface = Native.load("./gcd/build/libmodel.so", classOf[ModelInterface], opts).asInstanceOf[ModelInterface]


  given state: ModelState = ModelState(nInPorts, nOutPorts)

  var cycleRun = 0L

  def ticker(guard: Either[Int, (Output, Long)]): Unit = {
    val (inIds, inVals) = state.getInChanges()
    guard match {
      case Left(n) => {
        model.set_tick_get(inIds, inVals, inIds.length, state.outPorts, n)
        cycleRun += n
        println(s"running $n cycles")
      }
      case Right((out, value)) => {
        val elapsedCycles = model.set_tick_get_until_equal(inIds, inVals, inIds.length, state.outPorts, out.id, value)
        cycleRun += elapsedCycles
        println(s"ran $elapsedCycles cycles until ${out.id} == $value")
      }
    }
  }

  given tick: (Either[Int, (Output, Long)] => Unit) = ticker

  def setup(): Unit = model.sim_init()
  def close(): Unit = {
    model.sim_close()
    model.unload()
    println("Model closed")
  }

}

class GCD extends Model(3,2) {
  val a = Input(0)
  val b = Input(1)
  val clock = Clock()
  val loadValues = Input(2)
  val isValid = Output(0)
  val result = Output(1)
}

object Sim extends App {

  val gcd = GCD()

  val startTime = System.nanoTime()

  gcd.setup()

  println(s"Cycle run: ${gcd.cycleRun}")

  gcd.a.poke(0x4444)
  gcd.b.poke(0x700C)
  gcd.loadValues.poke(1)

  gcd.clock.tick()

  gcd.loadValues.poke(0)

  gcd.clock.tickUntil(gcd.isValid, 1)

  val endTime = System.nanoTime()
  val duration = (endTime - startTime) / 1e6 // Convert to milliseconds

  println(s"Result: ${gcd.result.peek()}")
  println(s"Cycle run: ${gcd.cycleRun}")
  println(s"Execution time: $duration ms")

  gcd.close()

}

*/