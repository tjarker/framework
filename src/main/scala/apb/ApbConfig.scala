package apb

class ApbConfig {

  val addrWidth = 32
  val dataWidth = 32
  val strobeWidth = math.floor(dataWidth / 8.0).toInt

}
