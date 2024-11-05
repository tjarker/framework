package framework.macros

import scala.quoted.*
import scala.annotation.experimental

object Naming {

  inline def enclosingTermName: String = ${ enclosingTermNameImpl }

  def enclosingTermNameImpl(using Quotes): Expr[String] = {
    import quotes.reflect._
    def enclosingTerm(sym: Symbol): Symbol = {
      sym match
        case sym if sym.flags.is(Flags.Macro) => enclosingTerm(sym.owner)
        case sym if !sym.isTerm               => enclosingTerm(sym.owner)
        case _                                => sym
    }
    Expr(enclosingTerm(Symbol.spliceOwner).name)
  }

}