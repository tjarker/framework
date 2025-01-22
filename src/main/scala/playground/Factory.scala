package playground

import framework.Component

abstract class UVMComponent {
  def name: String

}

class Factory[T <: UVMComponent] {

  object Type {
    def create(name: String): T = ??? // FactoryRegistration.createImpl[T](name)
  }

}

import scala.reflect.ClassTag
import scala.collection.mutable

object UVMFactory {

  private val tagMapping = mutable.Map[ClassTag[?], ClassTag[?]]()

  // Register a component type
  def register[T <: UVMComponent: ClassTag]: Unit = {
    val tag = implicitly[ClassTag[T]]
    println(s"Registering component: ${tag.runtimeClass.getName}")
    tagMapping(tag) = tag
  }

  def create[T <: UVMComponent: ClassTag](name: String): T = {
    val tag = implicitly[ClassTag[T]]
    val realTag = tagMapping(tag)
    realTag.runtimeClass
      .getDeclaredConstructor(classOf[String])
      .newInstance(name)
      .asInstanceOf[T]
  }

  def createUnsafe[T <: UVMComponent: ClassTag](args: Any*): T = {
    val tag = implicitly[ClassTag[T]]
    val realTag = tagMapping(tag)
    println(args.map(_.getClass).mkString(","))
    realTag.runtimeClass
      .getDeclaredConstructor(args.map(_.getClass)*)
      .newInstance(args*)
      .asInstanceOf[T]
  }

  def registerOverride[T <: UVMComponent: ClassTag, U <: T: ClassTag]: Unit = {
    val tag = implicitly[ClassTag[T]]
    val realTag = tagMapping(tag)
    tagMapping(tag) = implicitly[ClassTag[U]]
  }

}

// Define sample Driver and Monitor components
class Driver(val name: String, val msg: String) extends UVMComponent {
  def drive(): Unit = println(s"$name is driving with message: $msg.")
}

class Monitor(val name: String) extends UVMComponent {
  def monitor(): Unit = println(s"$name is monitoring.")
}

@main def testFactory(): Unit = {

// Register components in the factory by type
  UVMFactory.register[Driver]
  UVMFactory.register[Monitor]

// Create components using the factory by type
  val driver = UVMFactory.createUnsafe[Driver]("driver1", "hello")
  val monitor = UVMFactory.create[Monitor]("monitor1")

// Override the Driver with a new implementation
  class CustomDriver(name: String) extends Driver(name, "custom message") {
    override def drive(): Unit = println(s"$name is custom driving.")
  }

  UVMFactory.registerOverride[Driver, CustomDriver]

// Create the overridden driver and test it
  val customDriver = UVMFactory.create[Driver]("driver2")

  driver.drive()
  monitor.monitor()
  customDriver.drive()

}

trait Comp {
  var name: String = ""
}

trait ComponentFactory[A, T <: Comp] {

  def construct(c: A): T

  var constructor = this.construct

  def create(name: String, arg: A): T = {
    val c = constructor(arg)
    c.name = name
    c
  }

  inline def create(arg: A): T =
    create(framework.macros.Naming.enclosingTermName, arg)

  def overrideFactory[B, T2 <: T](
      f: ComponentFactory[B, T2],
      c: A => B
  ): Unit = {
    constructor = (a: A) => f.constructor(c(a))
  }

  def overrideFactory[T2 <: T](f: ComponentFactory[A, T2]): Unit = {
    constructor = f.constructor
  }

}

class MyThing(msg: String) extends Comp {
  def hello(): Unit = println(s"Hello $name, $msg!")
}

object MyThing extends ComponentFactory[String, MyThing] {
  def construct(c: String): MyThing = new MyThing(c)
}

class CustomThing(n: String, num: Int) extends MyThing(n) {
  override def hello(): Unit = println(s"Custom hello $name, $n! $num")
}

object CustomThing extends ComponentFactory[(String, Int), CustomThing] {
  def construct(c: (String, Int)): CustomThing = new CustomThing(c._1, c._2)
}

class OtherThing(msg: String) extends MyThing(msg) {
  override def hello(): Unit = println(s"Other hello $name, $msg!")
}

object OtherThing extends ComponentFactory[String, OtherThing] {
  def construct(c: String): OtherThing = new OtherThing(c)
}

@main def testFactoryy(): Unit = {
  val thing = MyThing.create("i am the original")
  thing.hello()

  MyThing.overrideFactory(CustomThing, s => (s, 42))

  val customThing = MyThing.create("i have a different constructor")
  customThing.hello()

  MyThing.overrideFactory(OtherThing)

  val customThing2 = CustomThing.create("i have a different constructor" -> 128)
  customThing2.hello()

  val otherThing = MyThing.create("customName", "i have the same constructor as the original")
  otherThing.hello()
}

//=========================================================

inline def create[T <: Comp, A <: Tuple](arg: A)(using FactoryProvider[A,T]): T = 
  summon[FactoryProvider[A,T]].create(arg)

trait FactoryProvider[A <: Tuple, T <: Comp] {
  def createInst(arg: A): T

  inline def create(arg: A): T = {
    val inst = createInst(arg)
    inst.name = framework.macros.Naming.enclosingTermName
    inst
  }
}



@main def testGivenFactory(): Unit = {

  given FactoryProvider[(String, Int), CustomThing] with {
    def createInst(c: (String, Int)) = CustomThing.create(c)
  }

  val customThing = create("i have a different constructor" -> 128)
  customThing.hello()

}
