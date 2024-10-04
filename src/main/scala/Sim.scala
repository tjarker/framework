
import com.sun.jna.{InvocationMapper, Library, Native, NativeLibrary}

import java.lang.reflect.{InvocationHandler, Method}



trait ModelInterface extends Library {
  def createSimContext(name: String, waveFile: String, timeResolution: String): Long
  def destroySimContext(ctx: Long): Unit
  def setInput(ctx: Long, id: Long, value: Long): Unit
  def getOutput(ctx: Long, id: Long): Long
  def tick(ctx: Long): Unit
}


object Sim extends App {
  val model = Native.load("./gcd/build/libmodel.so", classOf[ModelInterface]).asInstanceOf[ModelInterface]

  val ctx = model.createSimContext("GCD", "gcd.vcd", "1ns")

  model.tick(ctx)
  model.tick(ctx)

  model.setInput(ctx, 0, 0x4444)
  model.setInput(ctx, 1, 0x700C)
  model.setInput(ctx, 2, 1)

  model.tick(ctx)

  model.setInput(ctx, 2, 0)

  model.tick(ctx)

  val ctx2 = model.createSimContext("GCD", "gcd2.vcd", "10ns")

  model.setInput(ctx2, 0, 0x1343)
  model.setInput(ctx2, 1, 0x43211)
  model.setInput(ctx2, 2, 1)

  model.tick(ctx2)

  model.setInput(ctx2, 2, 0)

  model.tick(ctx2)

  while (model.getOutput(ctx2, 0) == 0) {
    model.tick(ctx2)
  }

  println(s"Result for model 2: ${model.getOutput(ctx2, 1)}")

  model.destroySimContext(ctx2)


  while (model.getOutput(ctx, 0) == 0) {
    model.tick(ctx)
  }

  println(s"Result for model 1: ${model.getOutput(ctx, 1)}")

  model.destroySimContext(ctx)
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
    for (i <- changes._1) inChanged(i.toInt) = false
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