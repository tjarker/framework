package apb

import framework.*


class ApbEnv(using Hierarchy) extends Component {

  val agent = Factory.create[ApbAgent]

}