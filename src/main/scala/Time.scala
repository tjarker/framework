import scala.language.postfixOps

object Time {

  enum TimeUnit(val exp: Int) {
    case fs extends TimeUnit(0)
    case ps extends TimeUnit(3)
    case ns extends TimeUnit(6)
    case us extends TimeUnit(9)
    case ms extends TimeUnit(12)
    case s extends TimeUnit(15)
  }

  case class Time(value: Long) extends Ordered[Time] {

    def +(that: Time): Time = {
      Time(value + that.value)
    }
    def -(that: Time): Time = {
      Time(value - that.value)
    }

    def toString(unit: TimeUnit): String = {
      val exp = unit.exp
      val v = value / math.pow(10, exp)
      s"%.3f ${unit}".format(v)
    }

    private def findBestTimeUnit(t: Time, unit: TimeUnit): TimeUnit = {
      val v = value / math.pow(10, unit.exp)
      if (unit == TimeUnit.s || v < 1000) {
        unit
      } else {
        findBestTimeUnit(t, TimeUnit.values(unit.ordinal + 1))
      }
    }

    override def toString(): String = {
      toString(findBestTimeUnit(this, TimeUnit.fs))
    }

    def valueIn(unit: Time): Long = {
      value / unit.value
    }

    override def compare(that: Time): Int = {
      this.value.compare(that.value)
    }
  }

  object Time {
    def apply(value: Long, unit: TimeUnit): Time = Time(
      value * math.pow(10, unit.exp).toLong
    )
  }

  extension (i: Int) {
    def fs: Time = Time(i, TimeUnit.fs)
    def ps: Time = Time(i, TimeUnit.ps)
    def ns: Time = Time(i, TimeUnit.ns)
    def us: Time = Time(i, TimeUnit.us)
    def ms: Time = Time(i, TimeUnit.ms)
    def s: Time = Time(i, TimeUnit.s)
  }

}
