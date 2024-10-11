package framwork

package object types {

  given Context = DefaultContext()

  case class Width(w: Int)
  extension (w: Int) {
    def W: Width = Width(w)
  }

  trait Bits {
    def width: Width
  }

  class UInt(value: BigInt, val width: Width) extends Bits

  object UInt {
    def apply(w: Width)(using ctx: Context): HardwareType[UInt] = {
      val t = new HardwareType[UInt] {
        val width = w
        def materialize: UInt = new UInt(0, w)
      }
      ctx.register(t)
      t
    }
  }

  class Clock() extends Bits {
    val width = 1.W
  }

  class Reset() extends Bits {
    val width = 1.W
  }

  class Bool(value: Boolean) extends Bits {
    val width = 1.W
  }

  object Bool {
    def apply()(using ctx: Context): HardwareType[Bool] = {
      val t = new HardwareType[Bool] {
        val width = 1.W
        def materialize: Bool = new Bool(false)
      }
      ctx.register(t)
      t
    }
  }

  class Bundle extends Bits {

    val ctx = BundleContext()
    given Context = ctx

    def show(): Unit = {
      println(ctx.fields)
    }

    def width: Width = {
      val w = ctx.fields.map(_.width.w).sum
      Width(w)
    }

  }

  class MyBundle extends Bundle {
    val a = UInt(8.W)
    val b = UInt(8.W)
  }

  trait Port[T <: Bits] extends HardwareType[T] {}

  class Input[T <: Bits](val t: HardwareType[T]) extends Port[T] {
    def poke(value: T): Unit = {
      println(s"poke ${t} ${value}")
    }
    val width = t.width
    def materialize: T = {
      t.materialize
    }
  }

  object Input {
    def apply[T <: Bits](
        t: HardwareType[T]
    )(using ctx: ModuleContext): Input[T] = {
      val i = new Input(t)
      ctx.register(i)
      i
    }

    def unapply[T <: Bits](input: Input[T]): Option[HardwareType[T]] = {
      Some(input.t)
    }
  }

  class Output[T <: Bits](val t: HardwareType[T]) extends Port[T] {
    def peek(): T = {
      println(s"peek ${t}")
      t.materialize
    }
    val width = t.width
    def materialize: T = {
      t.materialize
    }
  }

  object Output {
    def apply[T <: Bits](
        t: HardwareType[T]
    )(using ctx: ModuleContext): Output[T] = {
      val o = new Output(t)
      ctx.register(o)
      o
    }

    def unapply[T <: Bits](output: Output[T]): Option[HardwareType[T]] = {
      Some(output.t)
    }
  }

  class ClockPort() extends Port[Clock] {
    val width = 1.W
    def materialize: Clock = new Clock()
  }

  object ClockPort {
    def apply()(using ctx: ModuleContext): ClockPort = {
      val c = new ClockPort()
      ctx.register(c)
      c
    }
  }

  class ResetPort() extends Port[Reset] {
    val width = 1.W
    def materialize: Reset = new Reset()
  }

}
