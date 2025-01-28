package framework


object Test {

  def run[M <: ModuleInterface](dut: M, res: Time, wave: Option[String] = None)(testConstructor: Hierarchy ?=> M => Test): Unit = {
    Simulation(dut, res, debug = false, wave = wave) { dut =>
      val test = Comp.root(testConstructor(dut))
      Phase.run(test)
      Logger.info(s"${"=" * 40} Starting Reset Phase ${"=" * 40}")
      Phase.reset(test)
      Logger.info(s"${"=" * 40} Starting Test Phase ${"=" * 40}")
      Phase.test(test)
      Logger.info(s"${"=" * 40} Starting Report Phase ${"=" * 40}")
      Phase.report(test)
      Logger.info(s"${"=" * 40} Finished ${"=" * 40}")
    }
  }

}



abstract class Test(using Hierarchy) extends Component with TestPhase {


}