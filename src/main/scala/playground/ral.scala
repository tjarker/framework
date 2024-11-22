package playground

import scala.reflect.ClassTag



trait readable {
  def read(): Int
}

trait writable {
  def write(value: Int): Unit
}

trait Register

trait ReadOnly extends Register, readable

trait WriteOnly extends Register, writable

trait ReadWrite extends Register, readable, writable

trait WriteClears extends Register, writable

trait WRC extends Register, readable, writable


trait FieldGetter {
  def getFieldsOfType[T](using ct: ClassTag[T]): Seq[T] = {
    this.getClass.getDeclaredFields.toIndexedSeq.filter(field => ct.runtimeClass.isAssignableFrom(field.getType))
      .map { field =>
        field.setAccessible(true) // Make private fields accessible
        field.get(this).asInstanceOf[T]
      }
  }
}


@main def ral(): Unit = {

  case class Thing(name: String)
  
  class MyClass extends FieldGetter {
    val a = Thing("a")
    val b = 1
    val c = Thing("c")
    val d = 2
  }

  val myClass = MyClass()

  val fields = myClass.getFieldsOfType[Thing]

  println(fields)

}