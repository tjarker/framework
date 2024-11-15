import com.sun.jna.Native

import scala.collection.mutable

import gears.async.*

import framework.*
import types.*
import Time.*

import java.nio.file.Path

@main def GcdSim(): Unit = {

  class GCD extends Module("./gcd/build/gcd_model.so") {

    val clock = ClockPort(10.ns)
    val reset = ResetPort()
    val req = Input(Bool())
    val ack = Output(Bool())
    val loadVal = Input(UInt(128.W))
    val result = Output(UInt(128.W))

    domain(clock, reset)(
      req, ack, loadVal, result
    )
  }




  def transact(gcd: GCD, value: BigInt)(using Sim, Async): BigInt = {
    gcd.loadVal.poke(value)
    gcd.req.poke(1)

    while (gcd.ack.peek == 0) {
      gcd.clock.step()
    }

    val res = gcd.result.peek

    gcd.req.poke(0)

    while (gcd.ack.peek != 0) {
      gcd.clock.step()
    }

    res
  }

  Simulation(GCD(), 1.ns) { gcd => 
  
    gcd.reset.assert()
    gcd.req.poke(0)
    gcd.loadVal.poke(0)

    gcd.clock.step()

    gcd.reset.deassert()

    gcd.clock.step()

    throw new Exception("Not implemented")

    transact(gcd, BigInt("F123456789ABCDEF123456789ABCDEF1", 16))

    val res = transact(gcd, BigInt("123456789ABCDEF123456789ABCDEF12", 16))

    println(s"Result: ${res}")

  }

}
