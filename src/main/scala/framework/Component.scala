package framework

import gears.async.Async

import scala.collection.mutable

import scala.reflect.ClassTag
import gears.async.*

trait Phase 

object Phase {
  def run(c: Component)(using Sim, Async.Spawn): Future[?] = {
    var f: Future[?] = null
    c match
      case r: RunPhase => f = fork { r.run() }
      case _ =>
    c.getChildren.foreach(run)
    f
  }

  def report(c: Component): Unit = {
    c match
      case r: ReportPhase => r.report()
      case _ =>
    c.getChildren.foreach(report)
  }

  def reset(c: Component)(using Sim, Async.Spawn): Unit = {
    c match
      case r: ResetPhase => r.reset()
      case _ =>
    c.getChildren.foreach(reset)
  }
  
}

trait RunPhase extends Phase {
  def run()(using Sim, Async.Spawn): Unit
}
trait ReportPhase extends Phase {
  def report(): Unit
}

trait ResetPhase extends Phase {
  def reset()(using Sim, Async.Spawn): Unit
}

object Component {
  def root[C <: Component](c: C): C = {
    c.hierarchy = Hierarchy(null, mutable.ListBuffer())
    c.name = "root"
    c.initComponentHierarchy()
    Logger.info("sim", "done initializing component hierarchy")
    c
  }
}

case class Hierarchy(parent: Component, children: mutable.ListBuffer[Component])

trait Component extends Reportable {

  private[framework] var name: String = ""
  private[framework] var hierarchy: Hierarchy = null

  private[framework] def getChildren = hierarchy.children
  private[framework] def getParent = hierarchy.parent

  
  private[framework] def initComponentHierarchy()(using ct: ClassTag[Component]): Unit = {
    println(s"${this.name}:")

    def getAllFields(clazz: Class[?]): Seq[java.lang.reflect.Field] = {
      if (clazz == null || clazz == classOf[Object]) {
        Seq.empty
      } else {
        // Collect fields from the current class
        val fieldsInClass = clazz.getDeclaredFields.toIndexedSeq
        // Collect fields from the superclasses and traits
        val fieldsInSuperclass = getAllFields(clazz.getSuperclass)
        // Collect fields from the traits, which are part of the class hierarchy
        val fieldsInInterfaces = clazz.getInterfaces.flatMap(getAllFields)
        
        fieldsInClass ++ fieldsInSuperclass ++ fieldsInInterfaces
      }
    }


    getAllFields(this.getClass).filter(field => ct.runtimeClass.isAssignableFrom(field.getType))
      .foreach { field =>
        println(field.toString())
        field.setAccessible(true) // Make private fields accessible
        val c = field.get(this).asInstanceOf[Component]
        c.hierarchy = Hierarchy(this, mutable.ListBuffer())
        c.name = field.getName
        this.hierarchy.children += c
        c.initComponentHierarchy()
      }
  }
  

  override def toString(): String = 
    if (hierarchy.parent != null) s"${hierarchy.parent.toString()}.${name}"
    else name
}

trait ExitConsensus {
  def raiseObjection(): Unit
  def dropObjection(): Unit
  def registerCondition(condition: => Boolean): Unit
}




