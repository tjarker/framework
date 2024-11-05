package playground

object FactoryRegistration {

  import scala.quoted.*

  inline def createImpl[T <: UVMComponent](name: String): T = ${ createImplImpl[T]('name) }

  def createImplImpl[T <: UVMComponent: Type](name: Expr[String])(using Quotes): Expr[T] = ???

}



