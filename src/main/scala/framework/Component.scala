package framework

import gears.async.Async

import scala.collection.mutable

import scala.reflect.ClassTag
import gears.async.Async
import gears.async.ChannelMultiplexer

import Result.*

import java.lang.reflect.Modifier
import framework.macros.Naming

class CompBuilder(using Hierarchy) {

  val params = mutable.Map[Any, Any]()
  val overrides = mutable.Map[ClassTag[?], ClassTag[?]]()

  def withParams(kv: (Any, Any)*): CompBuilder = {
    params.addAll(kv)
    this
  }

  def withOverride[T <: Component: ClassTag, U <: T: ClassTag]: CompBuilder = {
    overrides.put(summon[ClassTag[T]], summon[ClassTag[U]])
    this
  }

  inline def create[T <: Component: ClassTag]: T = {
    Comp.create[T](Naming.enclosingTermName, params.toMap, overrides.toMap)
  }

  def create[T <: Component: ClassTag](name: String): T = {
    Comp.create[T](name, params.toMap, overrides.toMap)
  }

}

object Comp {

  def builder(using Hierarchy): CompBuilder = new CompBuilder

  def root[C <: Component: ClassTag](c: Hierarchy ?=> C): C = {

    val className = summon[ClassTag[C]].runtimeClass.getSimpleName()
    val instanceName = s"${className.head.toLower}${className.tail}"

    val h =
      ComponentHierarchy(instanceName, null)
    val component = c(using h)

    component
  }

  def apply[T <: Component](
      name: String,
      c: Hierarchy ?=> T,
      params: Map[Any, Any],
      overrides: Map[ClassTag[?], ClassTag[?]]
  )(using
      Hierarchy
  ): T = {

    val h = summon[Hierarchy]
    val newHierarchy = ComponentHierarchy(name, h.self)

    params.foreach(kv => newHierarchy.setConfig(kv._1, kv._2))
    overrides.foreach(kv => newHierarchy.setTypeOverride(kv._1, kv._2))

    val newComponent = c(using newHierarchy)

    h.addChild(newComponent)

    newComponent
  }

  def apply[T <: Component](name: String, c: Hierarchy ?=> T)(using
      Hierarchy
  ): T = {
    apply(name, c, Map.empty, Map.empty)
  }

  inline def apply[T <: Component](c: Hierarchy ?=> T)(using
      Hierarchy
  ): T = {
    apply(Naming.enclosingTermName, c)
  }

  def overrideType[T <: Component: ClassTag, U <: T: ClassTag](using
      Hierarchy
  ): Unit = {
    summon[Hierarchy].setTypeOverride(summon[ClassTag[T]], summon[ClassTag[U]])
  }

  def create[T <: Component: ClassTag](
      name: String,
      params: Map[Any, Any],
      overrides: Map[ClassTag[?], ClassTag[?]]
  )(using
      Hierarchy
  ): T = {

    val h = summon[Hierarchy]
    val classTag =
      h.tryGetTypeOverride(summon[ClassTag[T]]).getOrElse(summon[ClassTag[T]])

    Comp(
      name,
      classTag.runtimeClass
        .getConstructor(classOf[Hierarchy])
        .newInstance(summon[Hierarchy])
        .asInstanceOf[T],
      params,
      overrides
    )
  }

  def create[T <: Component: ClassTag](name: String)(using Hierarchy): T = {
    create[T](name, Map.empty, Map.empty)
  }

  inline def create[T <: Component: ClassTag](using Hierarchy): T = {
    create[T](Naming.enclosingTermName)
  }

  def set(kv: (Any, Any))(using Hierarchy): Unit = {
    summon[Hierarchy].setConfig(kv._1, kv._2)
  }

  def tryGet(key: Any)(using Hierarchy): Option[Any] = {
    summon[Hierarchy].getConfig(key)
  }

  def get(key: Any)(using Hierarchy): Any = {
    tryGet(key).getOrElse(throw new Exception(s"Key $key not found"))
  }

  def getOrElse(key: Any, default: Any)(using Hierarchy): Any = {
    tryGet(key).getOrElse(default)
  }

  def getConfigs()(using Hierarchy): Map[Any, Any] =
    summon[Hierarchy].getConfigs

  def printHierarchy(c: Component, indent: Int = 0): Unit = {
    val spaces = " " * indent
    println(s"${spaces}${c.hierarchy.name}")
    c.hierarchy.getChildren.foreach { child =>
      printHierarchy(child, indent + 2)
    }
  }
}

trait Component(using Hierarchy) extends Reportable {

  val hierarchy = summon[Hierarchy]
  val name = hierarchy.name
  hierarchy.self = this

  val banner =
    s"""==Component${"=" * (50 - 9 - 2)}
    | - Name: $name
    | - Type: ${this.getClass.getSimpleName}
    | - Config:
    |     ${Comp
        .getConfigs()
        .map(kv => s" - ${kv._1} -> ${kv._2}")
        .mkString("\n     ")}
    | - Overrides:
    |     ${hierarchy.getOverrides
        .map(kv =>
          s" - ${kv._1.runtimeClass.getSimpleName()} -> ${kv._2.runtimeClass.getSimpleName()}"
        )
        .mkString("\n     ")} 
    |${"=" * 50}""".stripMargin

  info(banner)

  override def toString(): String =
    if (hierarchy.parent != null)
      s"${hierarchy.parent.toString()}.${hierarchy.name}"
    else hierarchy.name
}

class Child(using Hierarchy) extends Component {
  info("Child")
}

class ChildDerived(using Hierarchy) extends Child {
  info("ChildDerived")
  info(Comp.get("key"))
  Comp.set("key" -> "child")
  info(Comp.get("key"))
}

class Parent(using Hierarchy) extends Component {
  info("Parent")
  info(Comp.get("key"))
  Comp.set("key" -> "parent")

  val child = Comp.builder
    .withParams(
      "key" -> "parent",
      "key2" -> "child2"
    )
    .withOverride[Child, ChildDerived]
    .create[Child]

  info(Comp.get("key"))
}

@main def testComponent(): Unit = {
  val root = Comp.root {

    Comp.set("key" -> "top")

    new Parent
  }

  Comp.printHierarchy(root)
}
