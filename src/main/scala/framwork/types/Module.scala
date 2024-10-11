package framwork.types

trait Module {
  val ctx = ModuleContext()
  given Context = ctx
  given ModuleContext = ctx

}
