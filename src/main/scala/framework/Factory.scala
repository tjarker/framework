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
  def parent: Component
  var self: Component = null
  def addChild(c: Component): Unit
  def getChildren: Seq[Component]
  def name: String
}

type OverrideMap = mutable.Map[ClassTag[?], ClassTag[?]]
type ConfigMap = mutable.Map[Any, Any]

class ComponentHierarchy(
    val name: String,
    val parent: Component
) extends Hierarchy {

  val overrides = mutable.Map[ClassTag[?], ClassTag[?]]()
  val config = mutable.Map[Any, Any]()

  val children = mutable.ListBuffer[Component]()

  def setTypeOverride(c: ClassTag[?], overrideTag: ClassTag[?]): Unit = {
    overrides.put(c, overrideTag)
  }

  def tryGetTypeOverride(c: ClassTag[?]): Option[ClassTag[?]] = {

    if (parent != null) {
      parent.hierarchy.tryGetTypeOverride(c) match {
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
      parent.hierarchy.getConfig(key) match {
        case Some(value) => return Some(value)
        case None        => 
      }
    }

    config.get(key)

  }

  def getConfigs: Map[Any, Any] = {
    if (parent != null) {
      parent.hierarchy.getConfigs ++ config.toSeq
    } else {
      config.toMap
    }
  }

  def getOverrides: Map[ClassTag[?], ClassTag[?]] = {
    if (parent != null) {
      parent.hierarchy.getOverrides ++ overrides
    } else {
      overrides.toMap
    }
  }

  def addChild(c: Component): Unit = {
    children += c
  }

  def getChildren: Seq[Component] = {
    children.toSeq
  }

}

