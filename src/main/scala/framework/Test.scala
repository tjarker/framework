package framework


object Test {

  def run[M <: ModuleInterface](dut: M, res: Time)(testConstructor: Hierarchy ?=> M => TestCase): Unit = {
    Simulation(dut, res, debug = true) { dut =>
      val test = Comp.root(testConstructor(dut))
      Phase.run(test)
      Phase.reset(test)
      Phase.test(test)
      Phase.report(test)
    }
  }

}



abstract class TestCase(using Hierarchy) extends Component with TestPhase {


}