package playground

object Regs {
  class RegisterModel {}

  class RegisterBlock {}

  class RegisterFile {}

  class Register {}

  class Field {}

}

@main def testRegs(): Unit = {

  import playground.Regs._
  import scala.reflect.Selectable.reflectiveSelectable

  class MyModel extends RegisterModel {
    val myBlock : RegisterBlock {val myFile: RegisterFile} = new RegisterBlock {
      val myFile = new RegisterFile {
        val myRegister = new Register {
          val myField = new Field {}
        }
      }
    }
  }

  val myModel = new MyModel

}

@main def helo = {

  class A {
    println("A")
  }
  class B extends A {
    println("B")
  }

  val b = new B


}