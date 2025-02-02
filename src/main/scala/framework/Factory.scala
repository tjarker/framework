package framework

import scala.reflect.ClassTag


class FactoryImpl {

  protected val contextParams = scala.collection.mutable.Map[Any, Any]()
  protected val contextOverrides = scala.collection.mutable.Map[ClassTag[?], ClassTag[?]]()


  private inline def create[T: ClassTag](
      name: String,
      params: Map[Any, Any],
      overrides: Map[ClassTag[?], ClassTag[?]]
  )(using Hierarchy): T = {
    HasHierarchyConstructor.checkHierarchyConstructor[T]

    val h = setupEnvironment(summon[Hierarchy], name, params ++ contextParams.toMap, overrides ++ contextOverrides.toMap)
    val classTag = getClassTag[T]
    val obj = classTag.runtimeClass
      .getDeclaredConstructor(classOf[Hierarchy])
      .newInstance(h)
      .asInstanceOf[T]

    obj match
      case c: Component => summon[Hierarchy].getComponent.get.children += c
      case _ => ()

    obj
    
  }

  inline def create[T: ClassTag](name: String, params: (Any,Any)*)(using Hierarchy): T = {
    create[T](name, Map.empty, Map.empty)
  }

  inline def create[T: ClassTag](using Hierarchy): T = {
    create[T](macros.Naming.enclosingTermName)
  }

  inline def create[T: ClassTag](params: (Any, Any)*)(using Hierarchy): T = {
    create[T](macros.Naming.enclosingTermName, params.toMap, Map.empty)
  }
  
  def setupEnvironment(
      old: Hierarchy,
      name: String,
      params: Map[Any, Any],
      overrides: Map[ClassTag[?], ClassTag[?]]
  ): Hierarchy = {
    val newParentHierarchy = old.copy()

    params.foreach(kv => newParentHierarchy.setConfig(kv._1, kv._2))
    overrides.foreach(kv => newParentHierarchy.setTypeOverride(kv._1, kv._2))

    ComponentHierarchy(name, newParentHierarchy)
  }

  def getClassTag[T: ClassTag](using Hierarchy): ClassTag[T] = {
    summon[Hierarchy]
      .tryGetTypeOverride(summon[ClassTag[T]])
      .getOrElse(summon[ClassTag[T]])
      .asInstanceOf[ClassTag[T]]
  }
}

class FactoryBuilder extends FactoryImpl {

  inline def withParams(params: (Any, Any)*): FactoryBuilder = {
    contextParams ++= params
    this
  }

  inline def withTypeOverride[T: ClassTag, U <: T: ClassTag]: FactoryBuilder = {
    HasHierarchyConstructor.checkHierarchyConstructor[T]
    HasHierarchyConstructor.checkHierarchyConstructor[U]
    contextOverrides += summon[ClassTag[T]] -> summon[ClassTag[U]]
    this
  }

  inline def withConfigOverride[T: ClassTag, U <: T: ClassTag]: FactoryBuilder = {
    HasEmptyConstructor.checkConstructor[T]
    HasEmptyConstructor.checkConstructor[U]
    contextOverrides += summon[ClassTag[T]] -> summon[ClassTag[U]]
    this
  }

}

object Factory extends FactoryImpl {

  inline def overrideType[T: ClassTag, U <: T: ClassTag](using Hierarchy): Unit = {
    HasHierarchyConstructor.checkHierarchyConstructor[T]
    HasHierarchyConstructor.checkHierarchyConstructor[U]
    summon[Hierarchy].setTypeOverride(summon[ClassTag[T]], summon[ClassTag[U]])
  }

  def builder: FactoryBuilder = new FactoryBuilder

  

}



