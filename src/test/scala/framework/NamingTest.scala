package framework

import org.scalatest.flatspec.AnyFlatSpec

class NamingTest extends AnyFlatSpec {

  it should "get enclosing term name" in {
    case class Test(name: String)
    val hello = Test(Naming.enclosingTermName)
    assert(hello == Test("hello"))
  }
  
}
