package framwork.types

import scala.collection.mutable

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