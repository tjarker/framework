package framework

import scala.reflect.ClassTag

object Config {


  def overrideConfig[T: ClassTag, U <: T: ClassTag](using Hierarchy): Unit = {
    summon[Hierarchy].setConfigOverride(summon[ClassTag[T]], summon[ClassTag[U]])
  }

  def get[T: ClassTag](using Hierarchy): T = {
    val tag = summon[Hierarchy].tryGetConfigOverride(summon[ClassTag[T]]).getOrElse(summon[ClassTag[T]])

    tag.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[T]
  }

  def set(kv: (Any, Any))(using Hierarchy): Unit = {
    summon[Hierarchy].setConfig(kv._1, kv._2)
  }

  def tryGet[T: ClassTag](key: Any)(using Hierarchy): Option[T] = {
    summon[Hierarchy].getConfig(key) match {
      case Some(value: T) => Some(value.asInstanceOf[T])
      case Some(_) => None
      case None => None
    }
  }

  def get[T: ClassTag](key: Any)(using Hierarchy): T = {
    tryGet(key).getOrElse(throw new Exception(s"Key $key not found"))
  }

  def getOrElse[T: ClassTag](key: Any, default: T)(using Hierarchy): T = {
    tryGet(key).getOrElse(default)
  }

  def getConfigs()(using Hierarchy): Map[Any, Any] =
    summon[Hierarchy].getConfigs

  def printConfigs(c: Component): Unit = {
    println(
      s"${c.name}: ${c.hierarchy.asInstanceOf[ComponentHierarchy].config}"
    )
    c.children.foreach { child =>
      printConfigs(child)
    }
  }


}
