import com.sun.jna.Native

import scala.collection.mutable

import framework.*
import framework.Time.*
import framework.Module
import framework.SimController
import framework.Types.*
import framework.ClockDomain

import java.nio.file.Path

@main def GcdSim(): Unit = {

  class Gcd extends Module("./gcd/build/gcd_model.so") {

    val name = "GCD"

    val clock = Input(Clock(10.ns))
    val reset = Input(Reset())
    val req = Input(Bool(), driveSkew = 4.ns)
    val ack = Output(Bool())
    val loadVal = Input(UInt(16.W), driveSkew = 3.ns)
    val result = Output(UInt(16.W))

    domains += ClockDomain(
      clock,
      Some(reset),
      mutable.ArrayBuffer[Input[Bits]](req, loadVal),
      mutable.ArrayBuffer[Output[Bits]](ack, result)
    )
  }


  val gcd = simulate(Gcd(), 1.ns)


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

  
  gcd.reset.assert()
  gcd.req.poke(0)
  gcd.loadVal.poke(0)

  gcd.clock.step()

  gcd.reset.deassert()

  gcd.clock.step()

  transact(gcd, 0x4444)

  val res = transact(gcd, 0x700C)

  println(s"Result: ${res}")

  gcd.destroy()

}
