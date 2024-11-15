package framework.macros

object FileContext {


  inline def apply(): String = ${ srcPointerImpl }


  import scala.quoted.*


  private def srcPointerImpl(using Quotes): Expr[String] = {
    import quotes.reflect.*
    
    val absPath = Position.ofMacroExpansion.sourceFile.getJPath.get.toString()
    val path = if (absPath.contains("src")) s"./src/${absPath.split("src").last}" else absPath
    val line = Position.ofMacroExpansion.startLine + 1

    val str = s"$path:$line"
    Expr(str)
  }

}
