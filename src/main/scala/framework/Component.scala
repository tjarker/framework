package framework

import gears.async.Async

import scala.collection.mutable

import scala.reflect.ClassTag
import gears.async.Async
import gears.async.ChannelMultiplexer

import Result.*

import java.lang.reflect.Modifier
import framework.macros.Naming

class CompBuilder {

  val params = mutable.Map[Any, Any]()
  val overrides = mutable.Map[ClassTag[?], ClassTag[?]]()

  def withConfig(kv: (Any, Any)*): CompBuilder = {
    params.addAll(kv)
    this
  }

  def withOverride[T <: Component: ClassTag, U <: T: ClassTag]: CompBuilder = {
    overrides.put(summon[ClassTag[T]], summon[ClassTag[U]])
    this
  }

  inline def create[T <: Component: ClassTag](using Hierarchy): T = {
    create[T](Naming.enclosingTermName)
  }

  def create[T <: Component: ClassTag](name: String)(using Hierarchy): T = {
    val h = summon[Hierarchy]
    val newParentHierarchy = h.copy()

    params.foreach(kv => newParentHierarchy.setConfig(kv._1, kv._2))
    overrides.foreach(kv => newParentHierarchy.setTypeOverride(kv._1, kv._2))

    {
      given Hierarchy = newParentHierarchy
      Comp.create[T](name)
    }
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
    val newHierarchy = ComponentHierarchy(name, h)

    params.foreach(kv => newHierarchy.setConfig(kv._1, kv._2))
    overrides.foreach(kv => newHierarchy.setTypeOverride(kv._1, kv._2))

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

    val c = Comp(
      name,
      {
        val i = classTag.runtimeClass
          .getConstructor(classOf[Hierarchy])

        println(s"got constructor $i")
          
        val c = i.newInstance(summon[Hierarchy])

        println(s"Creating $name with ${i.getClass.getSimpleName()}")
        c.asInstanceOf[T]
        
      },
      params,
      overrides
    )

    println(s"Created $name")
    c
  }

  def create[T <: Component: ClassTag](name: String)(using Hierarchy): T = {
    create[T](name, Map.empty, Map.empty)
  }

  inline def create[T <: Component: ClassTag](using Hierarchy): T = {
    create[T](Naming.enclosingTermName)
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

  def param[T](key: Any): T = Config
    .tryGet(key)
    .getOrElse(throw new Exception(s"Component $name expects parameter $key"))
    .asInstanceOf[T]
}


