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
    def getOutput(ctx: Pointer, id: Long): Long
    def tick(ctx: Pointer, cycles: Int): Unit
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

  val ctx = lib.createSimContext(
    dutName + "\u0000",
    waveFile + "\u0000",
    timeUnit.toString + "\u0000"
  )

  val inVals = collection.mutable.Map[Long, BigInt]()

  def tick(n: Int = 1) = lib.tick(ctx, n)

  private def setInput(id: Long, value: Long) = lib.setInput(ctx, id, value)

  private def getOutput(id: Long) = lib.getOutput(ctx, id)

  def peekInput(id: Long): BigInt = inVals(id)

  def pokeInput(id: Long, value: BigInt): Unit = {
    inVals(id) = value
    setInput(id, value.toLong)
  }

  def peekOutput(id: Long): BigInt = getOutput(id)

  def destroy() = lib.destroySimContext(ctx)

}
