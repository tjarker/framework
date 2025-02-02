package framework

import gears.async.Async

import scala.collection.mutable

import scala.reflect.ClassTag
import gears.async.Async
import gears.async.ChannelMultiplexer

import Result.*

import java.lang.reflect.Modifier
import framework.macros.Naming

object Comp {

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
    val newHierarchy = Factory.setupEnvironment(summon[Hierarchy], name, params, overrides)

    val newComponent = c(using newHierarchy)

    h.getComponent.get.children += newComponent

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

  

  

  def printHierarchy(c: Component, indent: Int = 0): Unit = {
    val spaces = " " * indent
    println(s"${spaces}${c.hierarchy.name}")
    c.children.foreach { child =>
      printHierarchy(child, indent + 2)
    }
  }

  
}

trait Component(using Hierarchy) extends Reportable {

  val children = mutable.ListBuffer[Component]()

  val hierarchy = summon[Hierarchy]
  val name = hierarchy.name
  hierarchy.setComponent(this)

  val banner =
    s"""==Component${"=" * (50 - 9 - 2)}
    | - Name: $name
    | - Type: ${this.getClass.getSimpleName}
    | - Config:
    |     ${Config
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
      s"${hierarchy.parent.getComponent.get.toString()}.${hierarchy.name}"
    else hierarchy.name


}


