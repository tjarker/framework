package apb

import framework.*

class ApbAgent(using Hierarchy) extends Component {

  val driver = Factory.create[ApbProducerDriver]
  val seq = Factory.create[Sequencer[ApbTransaction, ApbTransaction]]

  val monitor = Factory.create[ApbMonitor]
  val coverage = Factory.create[ApbCoverage]

  driver.port.connect(seq.port)
  monitor.addListeners(coverage)

}
