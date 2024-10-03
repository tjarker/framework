
import com.sun.jna.{Library, Native}

trait Lib extends Library {
  def hello(): Unit
}

trait ModelInterface extends Library {
  def sim_init(): Unit
  def sim_set(id: Long, value: Long): Unit
  def sim_get(id: Long): Long
  def sim_tick(): Unit
  def sim_close(): Unit
}

case class Port(id: Long)(using model: ModelInterface) {
  def poke(value: Long): Unit = model.sim_set(id, value)
  def peek(): Long = model.sim_get(id)
}

case class Clock(id: Long)(using model: ModelInterface) {
  def tick(): Unit = model.sim_tick()
}

abstract class Model {
  given model: ModelInterface = Native.load("./gcd/build/libmodel.so", classOf[ModelInterface]).asInstanceOf[ModelInterface]
  
  
  def setup(): Unit = model.sim_init()
  def close(): Unit = model.sim_close()
}

class GCD extends Model {
  val a = Port(0)
  val b = Port(1)
  val clock = Clock(2)
  val loadValues = Port(3)
  val isValid = Port(4)
  val result = Port(5)
}

object Sim extends App {

  val gcd = GCD()

  gcd.setup()

  gcd.a.poke(0x4444)
  gcd.b.poke(0x700C)
  gcd.loadValues.poke(1)

  gcd.clock.tick()

  gcd.loadValues.poke(0)

  while (gcd.isValid.peek() == 0) {
    gcd.clock.tick()
  }

  println(s"Result: ${gcd.result.peek()}")

  gcd.close()

}