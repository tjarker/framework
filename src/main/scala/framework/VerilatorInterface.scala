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
    def tick(ctx: Pointer): Unit
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

  def tick(n: Int = 1) = for (_ <- 0 until n) lib.tick(ctx)

  def setInput(id: Long, value: Long) = lib.setInput(ctx, id, value)

  def getOutput(id: Long) = lib.getOutput(ctx, id)

  def destroy() = lib.destroySimContext(ctx)

  def peekInput(id: Long): BigInt = 0

  def pokeInput(id: Long, value: BigInt): Unit = setInput(id, value.toLong)

  def peekOutput(id: Long): BigInt = getOutput(id)

  def stepClockDomain(cd: ClockDomain, steps: Int): Unit = {
    //println(s"Step $steps in $cd")
    for (_ <- 0 until steps) {
      lib.tick(ctx)
    }
  }

}
