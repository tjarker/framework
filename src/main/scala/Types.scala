
import scala.collection.mutable

object Types extends App {



    trait Context {
        def register[T <: Bits](t: HardwareType[T]): Unit
    }

    class BundleContext() extends Context {

        val fields = mutable.ArrayBuffer[HardwareType[Bits]]()

        override def register[T <: Bits](t: HardwareType[T]): Unit = {
            fields += t.asInstanceOf[HardwareType[Bits]]
        }
    }

    class DefaultContext() extends Context {
        override def register[T <: Bits](t: HardwareType[T]): Unit = {}
    }
    given Context = DefaultContext()

    trait Bits {
        def width: Width
    }

    trait HardwareType[T <: Bits] {
        def width: Width
        def produce: T

        override def toString(): String = {
            s"Type(${width})"
        }
    }

    case class Width(w: Int)

    extension (i: Int) {
        def W: Width = Width(i)
        def U(w: Width): UInt = new UInt(BigInt(i), w)
    }

    class UInt(value: BigInt, val width: Width) extends Bits

    object UInt {
        def apply(w: Width)(using ctx: Context): HardwareType[UInt] = {
            val t = new HardwareType[UInt] {
                val width = w
                def produce: UInt = new UInt(0,w)
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

    def wire[T <: Bits](a: HardwareType[T]): T = {
        a.produce
    }

    class MyBundle extends Bundle {
        val a = UInt(8.W)
        val b = UInt(8.W)
    }


    trait Port[T <: Bits] extends HardwareType[T] {
        
    }

    class Input[T <: Bits](val t: HardwareType[T]) extends Port[T] {
        def poke(value: T): Unit = {
            println(s"poke ${t} ${value}")
        }
        val width = t.width
        def produce: T  = {
            t.produce
        }
    }

    object Input {
        def apply[T <: Bits](t: HardwareType[T])(using ctx: ModuleContext): Input[T] = {
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
            t.produce
        }
        val width = t.width
        def produce: T  = {
            t.produce
        }
    }

    object Output {
        def apply[T <: Bits](t: HardwareType[T])(using ctx: ModuleContext): Output[T] = {
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
        def produce: Clock = new Clock()
    }

    object ClockPort {
        def apply()(using ctx: ModuleContext): ClockPort = {
            val c = new ClockPort()
            ctx.register(c)
            c
        }
    }


    class ModuleContext extends Context {
        val inputs = mutable.ArrayBuffer[HardwareType[Bits]]()
        val outputs = mutable.ArrayBuffer[HardwareType[Bits]]()
        val clocks = mutable.ArrayBuffer[ClockPort]()

        override def register[T <: Bits](t: HardwareType[T]): Unit = {

            t match {
                case Input(in) => inputs += in.asInstanceOf[HardwareType[Bits]]
                case Output(out) => outputs += out.asInstanceOf[HardwareType[Bits]]
                case c: ClockPort => clocks += c
                case _ => {}
            }

        }

        def inputMap: Map[HardwareType[Bits], Int] = {
            inputs.zipWithIndex.toMap
        }
        def outputMap: Map[HardwareType[Bits], Int] = {
            outputs.zipWithIndex.toMap
        }
        def clockMap: Map[ClockPort, Int] = {
            clocks.zipWithIndex.toMap
        }
    }

    abstract class Module {
        val ctx = ModuleContext()
        given Context = ctx
        given ModuleContext = ctx

    }

    class MyModule extends Module {
        val a = Input(UInt(16.W))
        val b = Output(UInt(8.W))
        val clk = ClockPort()
    }

    val m = new MyModule
    m.a.poke(16.U(16.W))
    m.b.peek()

    println(m.ctx.inputMap)
    println(m.ctx.outputMap)
    println(m.ctx.clockMap)

    val b = new MyBundle
    b.show()
    println(b.width)


}
