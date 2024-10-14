package framework.types

import framework.Module.ModuleBuilderContext

trait Port[+T <: Bits] {
  def width: Width
  val ctx: ModuleBuilderContext
  val name: String
}
