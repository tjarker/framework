import com.sun.jna.Native

import scala.collection.mutable

import framework.*
import framework.Time.*
import framework.Module
import framework.SimController
import framework.Types.*
import framework.ClockDomain

import java.nio.file.Path

object GcdSim extends App {

  class Gcd extends Module("./gcd/build/gcd_model.so") {

    val name = "GCD"

    val clock = Input("clock", Clock())
    val reset = Input("reset", Reset())
    val req = Input("req", Bool())
    val ack = Output("ack", Bool())
    val loadVal = Input("loadVal", UInt(16.W))
    val result = Output("result", UInt(16.W))

    domains += ClockDomain(
      clock,
      Some(reset),
      mutable.ArrayBuffer[Input[Bits]](reset, req, loadVal),
      mutable.ArrayBuffer[Output[Bits]](ack, result)
    )
  }


  val gcd = simulate(Gcd())

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
