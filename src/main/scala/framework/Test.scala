package framework


object Test {

  def run[M <: ModuleInterface](dut: M, res: Time, wave: Option[String] = None)(testConstructor: Hierarchy ?=> M => Test): Unit = {
    Simulation(dut, res, debug = true, wave = wave) { dut =>
      val test = Comp.root(testConstructor(dut))
      Phase.run(test)
      Phase.reset(test)
      Phase.test(test)
      Phase.report(test)
    }
  }

}



abstract class Test(using Hierarchy) extends Component with TestPhase {


}