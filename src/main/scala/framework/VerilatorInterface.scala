package framework

import com.sun.jna.*

import Time.*

object VerilatorInterface {

  trait NativeCalls extends Library {
    def createSimContext(
        name: String,
        waveFile: String,
        timeResolution: String
    ): Pointer
    def destroySimContext(ctx: Pointer): Unit
    def setInput(ctx: Pointer, id: Long, value: Long): Unit
    def setInputWide(ctx: Pointer, id: Long, value: Array[Long]): Unit
    def getOutput(ctx: Pointer, id: Long): Long
    def getOutputWide(ctx: Pointer, id: Long, value: Array[Long]): Unit
    def tick(ctx: Pointer, targetCycle: Long): Unit
  }

  class ModelInterface(m: Module, so: NativeLibrary) {

    val createSimContextHandle = so.getFunction(s"createSimContext_${m.name}")
    val destroySimContextHandle = so.getFunction(s"destroySimContext_${m.name}")
    val setInputHandle = so.getFunction(s"setInput_${m.name}")
    val setInputWideHandle = so.getFunction(s"setInputWide_${m.name}")
    val getOutputHandle = so.getFunction(s"getOutput_${m.name}")
    val getOutputWideHandle = so.getFunction(s"getOutputWide_${m.name}")
    val tickHandle = so.getFunction(s"tick_${m.name}")
    val quackHandle = so.getFunction(s"quack_${m.name}")

    def createInstance(name: String, waveFile: String, timeResolution: String): Pointer = {
      createSimContextHandle.invokePointer(Array(name, waveFile, timeResolution))
    }
    def destroyInstance(ctx: Pointer): Unit = {
      destroySimContextHandle.invoke(Array(ctx))
    }
    def setInput(ctx: Pointer, id: Long, value: Long): Unit = {
      setInputHandle.invoke(Array(ctx, id, value))
    }
    def setInputWide(ctx: Pointer, id: Long, value: Array[Int]): Unit = {
      setInputWideHandle.invokeVoid(Array(ctx, id, value))
    }
    def getOutput(ctx: Pointer, id: Long): Long = {
      getOutputHandle.invokeLong(Array(ctx, id))
    }
    def getOutputWide(ctx: Pointer, id: Long, value: Array[Int]): Unit = {
      getOutputWideHandle.invoke(Array(ctx, id, value))
    }
    def tick(ctx: Pointer, targetCycle: Long): Unit = {
      tickHandle.invoke(Array(ctx, targetCycle))
    }
    def quack(): Unit = {
      quackHandle.invoke(Array())
    }

  }
}

class VerilatorInterface(
    so: NativeLibrary,
    m: Module,
    waveFile: String, 
    timeUnit: Time
) {

  // TODO: caching should happen here!

  val lib = VerilatorInterface.ModelInterface(m, so)

  val log = Logger(false)

  val ctx = lib.createInstance(
    m.name + "\u0000",
    waveFile + "\u0000",
    timeUnit.toString + "\u0000"
  )

  val inVals = collection.mutable.Map[Long, BigInt]()
  m.inputs.foreach { case i => inVals(m.portToId(i)) = BigInt(0) }

  def tick(until: AbsoluteTime) = lib.tick(ctx, until.fs / timeUnit.fs)

  private def setInput(id: Long, value: BigInt, width: Int) = {

    if (width > 64) {
      log.info("verilator", s"Setting wide input $id with width $width to ${value.toString(16)} (${value.toInt32Array(width).map(_.toHexString).mkString(", ")})")
      lib.setInputWide(ctx, id, value.toInt32Array(width))
    } else {
      log.info("verilator", s"Setting input $id with width $width to $value")
      lib.setInput(ctx, id, value.toLong)
    }
  }

  extension (b: BigInt) {
    def toInt32Array(width: Int): Array[Int] = {
      val words = math.ceil(width / 32.0).toInt
      (0 until words).map(i => (b >> i * 32) & 0xFFFFFFFFL).map(_.toInt).toArray
    }
  }

  extension (l: Array[Int]) {
    def toBigInt: BigInt = {
      l.foldRight(BigInt(0))((x, acc) => (acc << 32) | (BigInt(x) & 0xFFFFFFFFL))
    }
  }

  private def getOutput(id: Long, width: Int): BigInt = {
    if (width > 64) {
      val arr = new Array[Int](width)
      lib.getOutputWide(ctx, id, arr)
      val r = arr.toBigInt
      log.info("verilator", s"Getting wide output $id as $r")
      r
    } else {
      val r = lib.getOutput(ctx, id)
      log.info("verilator", s"Getting output $id as $r")
      r
    }
  }

  def peekInput(id: Long): BigInt = inVals(id)

  def pokeInput(id: Long, value: BigInt, width: Int): Unit = {
    inVals(id) = value
    setInput(id, value, width)
  }

  def peekOutput(id: Long, width: Int): BigInt = getOutput(id, width)

  def destroy() = lib.destroyInstance(ctx)

}
