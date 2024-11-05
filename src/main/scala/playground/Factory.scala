package playground

abstract class UVMComponent {
  def name: String

}

class Factory[T <: UVMComponent] {

  object Type {
    def create(name: String): T = ???//FactoryRegistration.createImpl[T](name)
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

  def registerOverride[T <: UVMComponent: ClassTag, U <: T: ClassTag]: Unit = {
    val tag = implicitly[ClassTag[T]]
    val realTag = tagMapping(tag)
    tagMapping(tag) = implicitly[ClassTag[U]]
  }

}

// Define sample Driver and Monitor components
class Driver(val name: String) extends UVMComponent {
  def drive(): Unit = println(s"$name is driving.")
}

class Monitor(val name: String) extends UVMComponent {
  def monitor(): Unit = println(s"$name is monitoring.")
}

@main def testFactory(): Unit = {

// Register components in the factory by type
  //UVMFactory.register[Driver]
  //UVMFactory.register[Monitor]

// Create components using the factory by type
  val driver = UVMFactory.create[Driver]("driver1")
  val monitor = UVMFactory.create[Monitor]("monitor1")

// Override the Driver with a new implementation
  class CustomDriver(name: String) extends Driver(name) {
    override def drive(): Unit = println(s"$name is custom driving.")
  }

  UVMFactory.registerOverride[Driver, CustomDriver]

// Create the overridden driver and test it
  val customDriver = UVMFactory.create[Driver]("driver2")
  

  driver.drive()
  monitor.monitor()
  customDriver.drive()

}
