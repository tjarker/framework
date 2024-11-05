import com.sun.jna.Native

import scala.collection.mutable

import framework.*
import framework.Time.*
import framework.Module
import framework.SimController
import framework.types.*
import framework.ClockDomain

import java.nio.file.Path

@main def GcdSim(): Unit = {

  class Gcd extends Module("./gcd/build/gcd_model.so") {

    val name = "GCD"

    val clock = Input(Clock(10.ns))
    val reset = Input(Reset())
    val req = Input(Bool(), driveSkew = 4.ns)
    val ack = Output(Bool())
    val loadVal = Input(UInt(128.W), driveSkew = 3.ns)
    val result = Output(UInt(128.W))

    domains += ClockDomain(
      clock,
      Some(reset),
      mutable.ArrayBuffer[Input[Bits]](req, loadVal),
      mutable.ArrayBuffer[Output[Bits]](ack, result)
    )
  }




  def transact(gcd: Gcd, value: BigInt): BigInt = {
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

  Simulation(Gcd(), 1.ns, debug = false) { gcd => 
  
    gcd.reset.assert()
    gcd.req.poke(0)
    gcd.loadVal.poke(0)

    gcd.clock.step()

    gcd.reset.deassert()

    gcd.clock.step()

    transact(gcd, BigInt("F123456789ABCDEF123456789ABCDEF1", 16))

    val res = transact(gcd, BigInt("123456789ABCDEF123456789ABCDEF12", 16))

    println(s"Result: ${res}")

  }

}
