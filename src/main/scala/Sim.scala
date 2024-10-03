
import com.sun.jna.{Library, Native}

trait Lib extends Library {
  def hello(): Unit
}

trait ModelInterface extends Library {
  def sim_init(): Unit
  def sim_close(): Unit
  def set_tick_get(in_ids: Array[Long], in_vals: Array[Long], in_size: Int, out_vals: Array[Long], ticks: Long): Unit
  def set_tick_get_until_equal(in_ids: Array[Long], in_vals: Array[Long], in_size: Int, out_vals: Array[Long], port_id: Long, value: Long): Unit
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

abstract class Model(nInPorts: Int, nOutPorts: Int) {
  given model: ModelInterface = Native.load("./gcd/build/libmodel.so", classOf[ModelInterface]).asInstanceOf[ModelInterface]
  given state: ModelState = ModelState(nInPorts, nOutPorts)

  def ticker(guard: Either[Int, (Output, Long)]): Unit = {
    val (inIds, inVals) = state.getInChanges()
    guard match {
      case Left(n) => model.set_tick_get(inIds, inVals, inIds.length, state.outPorts, n)
      case Right((out, value)) => model.set_tick_get_until_equal(inIds, inVals, inIds.length, state.outPorts, out.id, value)
    }
  }

  given tick: (Either[Int, (Output, Long)] => Unit) = ticker

  def setup(): Unit = model.sim_init()
  def close(): Unit = model.sim_close()

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

  gcd.a.poke(0x4444)
  gcd.b.poke(0x700C)
  gcd.loadValues.poke(1)

  gcd.clock.tick()

  gcd.loadValues.poke(0)

  gcd.clock.tickUntil(gcd.isValid, 1)

  val endTime = System.nanoTime()
  val duration = (endTime - startTime) / 1e6 // Convert to milliseconds

  println(s"Result: ${gcd.result.peek()}")
  println(s"Execution time: $duration ms")

  gcd.close()

}