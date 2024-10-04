import scala.language.postfixOps

object Time {

  enum TimeUnit(val base: Int) {
    case ns extends TimeUnit(-9)
    case us extends TimeUnit(-6)
    case ms extends TimeUnit(-3)
    case s extends TimeUnit(0)
  }

  case class Time(value: Long, unit: TimeUnit) {
    def +(that: Time): Time = {
      Time(value + that.value * math.pow(10, unit.base - that.unit.base).toLong, unit)
    }
    def -(that: Time): Time = {
      Time(value - that.value * math.pow(10, unit.base - that.unit.base).toLong, unit)
    }

  }

  extension (i: Int) {
    def ns: Time = Time(i, TimeUnit.ns)
    def us: Time = Time(i, TimeUnit.us)
    def ms: Time = Time(i, TimeUnit.ms)
    def s: Time = Time(i, TimeUnit.s)
  }
  
  val a = 1 ns

}
