
import com.sun.jna._

import framwork.types._

object GcdSim extends App {



    val lib = Native.load("./gcd_hs/build/gcd_hs_model.so", classOf[ModelInterface])

    val ctx = lib.createSimContext("GCD", "gcd_hs.vcd", "1ns")

    def tick(n: Int = 1) = for (_ <- 0 until n) lib.tick(ctx)
    def reset(v: Long) = lib.setInput(ctx, 0, v)
    def req(v: Long) = lib.setInput(ctx, 1, v)
    def loadVal(v: Long) = lib.setInput(ctx, 2, v)

    def ack() = lib.getOutput(ctx, 0)
    def result() = lib.getOutput(ctx, 1)

    reset(1)
    req(0)
    loadVal(0)

    tick(2)

    reset(0)

    tick(2)

    loadVal(0x4444)
    req(1)

    tick(4)
    while (ack() == 0) tick()

    req(0)

    tick(4)
    while (ack() == 1) tick()



    loadVal(0x700C)
    req(1)

    while (ack() == 0) tick()
    println(s"Result: ${result()}")
    tick(4)

    req(0)

    tick(4)
    while (ack() == 1) tick()



    lib.destroySimContext(ctx)
  
}
