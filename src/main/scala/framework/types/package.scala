package framework


package object types {

  trait TypeContext {
    def register[T <: Bits](b: T): T
  }
  object NullContext extends TypeContext {
    def register[T <: Bits](b: T): T = b
  }

  case class Width(width: Int) {
    override def toString(): String = s"${width - 1}:0"
    def toInt: Int = width
  }

  extension (w: Int) {
    def W: Width = Width(w)
  }

  trait Bits {
    def width: Width
  }

  trait Data extends Bits

  extension [T <: Data](p: Port[T]) {
    def peek: BigInt = {
      Simulation.peek(p)
    }
  }

  extension [T <: Data](p: Input[T]) {
    def poke(value: BigInt): Unit = {
      Simulation.poke(p, value)
    }
  }

  extension (p: Input[Clock]) {
    def step(steps: Int = 1): Unit = {
      Simulation.step(p, steps)
    }
    def period = p.t.period
  }

  extension (p: Input[Reset]) {
    def assert(): Unit = {
      Simulation.poke(p, 1)
    }
    def deassert(): Unit = {
      Simulation.poke(p, 0)
    }
    def peek: BigInt = {
      Simulation.peek(p)
    }
  }
  
}
