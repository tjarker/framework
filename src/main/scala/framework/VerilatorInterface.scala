package framework

import com.sun.jna.{Library, Native, Pointer}

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
}

class VerilatorInterface(
    libPath: String,
    dutName: String,
    waveFile: String,
    timeUnit: Time
) {

  // TODO: caching should happen here!

  val lib = Native.load(libPath, classOf[VerilatorInterface.NativeCalls])

  val log = Logger(true)

  val ctx = lib.createSimContext(
    dutName + "\u0000",
    waveFile + "\u0000",
    timeUnit.toString + "\u0000"
  )

  val inVals = collection.mutable.Map[Long, BigInt]()

  def tick(until: AbsoluteTime) = lib.tick(ctx, until.fs / timeUnit.fs)

  private def setInput(id: Long, value: BigInt, width: Int) = {

    if (width > 64) {
      log.info("verilator", s"Setting wide input $id to $value")
      lib.setInputWide(ctx, id, value.toLongArray(width))
    } else {
      log.info("verilator", s"Setting input $id to $value")
      lib.setInput(ctx, id, value.toLong)
    }
  }

  extension (b: BigInt) {
    def toLongArray(width: Int): Array[Long] = {
      (0 until width).map(i => (b >> i * 64) & 0xFFFFFFFFFFFFFFFFL).map(_.toLong).toArray
    }
  }

  extension (l: Array[Long]) {
    def toBigInt: BigInt = {
      l.foldRight(BigInt(0))((x, acc) => (acc << 64) | BigInt(x))
    }
  }

  private def getOutput(id: Long, width: Int): BigInt = {
    if (width > 64) {
      val arr = new Array[Long](width)
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
    setInput(id, value.toLong, width)
  }

  def peekOutput(id: Long, width: Int): BigInt = getOutput(id, width)

  def destroy() = lib.destroySimContext(ctx)

}
