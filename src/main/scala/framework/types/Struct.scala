package framework.types

import scala.collection.mutable

import framework.types.*

object Struct {

  class StructContext extends TypeContext {
    val fields = mutable.ArrayBuffer[Bits]()
    def register[T <: Bits](b: T): T = {
      fields += b
      b
    }
  }

}

abstract class Struct extends Data {

  given ctx: Struct.StructContext = new Struct.StructContext

  lazy val width: Width = {
    val w = ctx.fields.map(_.width.width).sum
    w.W
  }
}

@main def testStruct() = {


  class MyStruct extends Struct {
    val a = UInt(8.W)
    val b = SInt(8.W)
  }

  val s = new MyStruct()
  println(s.ctx.fields)
  println(s.width)
}