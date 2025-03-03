package framework

import scala.annotation.targetName
import framework.Time.ps


case class DutFactory[M <: ModuleInterface](f: () => M) {
  def apply(): M = f()
}


class TestSuite {

  val tests = collection.mutable.Map[String, collection.mutable.ListBuffer[TestCase]]()


  case class TestCase(name: String, test: () => Unit)

  case class TestCaseBuilder[M <: ModuleInterface](desc: String, res: Time, wave: Option[String], m: () => M) {


    def in(block: (Sim, Async.Spawn) ?=> M => Unit): Unit = {
      tests(m().name) += TestCase(desc, () => {
        Simulation(m(), res, wave = wave) { dut =>
          block(dut)
        }
      })
    }

    def withTest(test: Hierarchy ?=> M => Test): Unit = {
      tests(m().name) += TestCase(desc, () => {
        Test.run(m(), res, wave)(test)
      })
    }

    def withResolution(res: Time): TestCaseBuilder[M] = this.copy(res = res)
    def withDump(wave: String): TestCaseBuilder[M] = this.copy(wave = Some(wave))

  }

  def test[M <: ModuleInterface](s: String)(using DutFactory[M]) = TestCaseBuilder(s, 1.ps, None, summon[DutFactory[M]].apply)


  def main(args: Array[String]): Unit = {


    if (args.nonEmpty) {

      val test = args.head
      val testcases = tests.flatMap(_._2).filter(_.name == test)
      if (testcases.isEmpty) {
        Logger.error(s"Test ${test} not found")
        return
      }

      testcases.foreach { testcase =>
        Logger.info(s"Running ${testcase.name}")
        try {
          testcase.test()
        } catch {
          case e: Exception => Logger.error(s"Test ${testcase.name} failed with ${e}")
        }
      }
      return

    }


    val status = collection.mutable.Map[TestCase, Option[Exception]]()
    tests.foreach { (dutName, testcases) =>
      Logger.info(s"Testing ${dutName}")
      testcases.foreach { testcase =>
        Logger.info(s"Running ${testcase.name}")
        try {
          testcase.test()
        } catch {
          case e: Exception =>  status(testcase) = Some(e) 
        }
      }
    }

    Logger.info(Logger.banner("Test Results"))

    tests.foreach { (dutName, testcases) =>
      Logger.info(s"- ${dutName}:")
      testcases.foreach { testcase =>
        status.get(testcase) match {
          case Some(e) => Logger.error(s" - ${testcase.name}: failed with ${e}")
          case None => Logger.info(s"  - ${testcase.name}: passed")
        }
      }
    }
  }

  def testModule[M <: ModuleInterface](dut: => M)(block: DutFactory[M] ?=> Any): Unit = {
    tests(dut.name) = collection.mutable.ListBuffer()
    block(using DutFactory(() => dut))
  }

}



object Test {

  def run[M <: ModuleInterface](dut: M, res: Time, wave: Option[String] = None)(testConstructor: Hierarchy ?=> M => Test): Unit = {
    Simulation(dut, res, debug = false, wave = wave) { dut =>
      val test = Comp.root(testConstructor(dut))
      Phase.run(test)
      Logger.info(Logger.banner("Starting Reset Phase"))
      Phase.reset(test)
      Logger.info(Logger.banner("Starting Test Phase"))
      Phase.test(test)
      Logger.info(Logger.banner("Starting Report Phase"))
      Phase.report(test)
      Logger.info(Logger.banner("Finished"))
    }
  }

}


abstract class Test(using Hierarchy) extends Component, TestPhase  {

}