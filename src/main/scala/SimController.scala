trait Port

class Input extends Port
class Output extends Port

class Clock(val low: Int, high: Int) extends Input {}

class Reset extends Input {}

class ClockDomain(
    val clock: Clock,
    val reset: Option[Reset],
    val ports: List[Port]
) {}

trait SimulationInterface {

  def set(port: Input, value: BigInt): Unit
  def get(port: Port): BigInt

  def tick(ticks: Int): Unit

  def step(cd: ClockDomain, steps: Int): Unit

}
