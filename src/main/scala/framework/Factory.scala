package framework

import scala.collection.mutable

import scala.reflect.ClassTag

trait Hierarchy {
  def setTypeOverride(c: ClassTag[?], overrideTag: ClassTag[?]): Unit
  def tryGetTypeOverride(c: ClassTag[?]): Option[ClassTag[?]]
  def getOverrides: Map[ClassTag[?], ClassTag[?]]
  def setConfig(key: Any, value: Any): Unit
  def getConfig(key: Any): Option[Any]
  def getConfigs: Map[Any, Any]
  def parent: Hierarchy
  def setComponent(c: Component): Unit
  def getComponent: Option[Component]
  def name: String
  def copy(): Hierarchy
}

type OverrideMap = mutable.Map[ClassTag[?], ClassTag[?]]
type ConfigMap = mutable.Map[Any, Any]

class ComponentHierarchy(
    val name: String,
    val parent: Hierarchy
) extends Hierarchy {

  val overrides = mutable.Map[ClassTag[?], ClassTag[?]]()
  val config = mutable.Map[Any, Any]()

  

  var component: Option[Component] = None

  def setComponent(c: Component): Unit = {
    component = Some(c)
  }

  def getComponent: Option[Component] = {
    component
  }

  def setTypeOverride(c: ClassTag[?], overrideTag: ClassTag[?]): Unit = {
    overrides.put(c, overrideTag)
  }

  def tryGetTypeOverride(c: ClassTag[?]): Option[ClassTag[?]] = {

    if (parent != null) {
      parent.tryGetTypeOverride(c) match {
        case Some(value) => return Some(value)
        case None        => 
      }
    }

    overrides.get(c)
  }

  def setConfig(key: Any, value: Any): Unit = {
    config.put(key, value)
  }

  def getConfig(key: Any): Option[Any] = {
    
    if (parent != null) {
      parent.getConfig(key) match {
        case Some(value) => return Some(value)
        case None        => 
      }
    }

    config.get(key)

  }

  def getConfigs: Map[Any, Any] = {
    if (parent != null) {
      val map = mutable.Map[Any, Any]()
      map ++= config
      parent.getConfigs.foreach(kv => map.put(kv._1, kv._2))
      map.toMap
    } else {
      config.toMap
    }
  }

  def getOverrides: Map[ClassTag[?], ClassTag[?]] = {
    if (parent != null) {
      val map = mutable.Map[ClassTag[?], ClassTag[?]]()
      map ++= overrides
      parent.getOverrides.foreach(kv => map.put(kv._1, kv._2))
      map.toMap
    } else {
      overrides.toMap
    }
  }

  def copy(): Hierarchy = {
    val h = new ComponentHierarchy(name, parent)
    h.overrides ++= overrides
    h.config ++= config
    h.setComponent(component.getOrElse(throw new Exception("Component not set in hierarchy.")))
    h
  }

}

