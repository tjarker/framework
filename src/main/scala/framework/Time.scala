package framework

object Time {

  enum TimeUnit(val exp: Int) {
    case fs extends TimeUnit(0)
    case ps extends TimeUnit(3)
    case ns extends TimeUnit(6)
    case us extends TimeUnit(9)
    case ms extends TimeUnit(12)
    case s extends TimeUnit(15)
  }

  extension (i: Int) {
    def fs: Time = Time(i, TimeUnit.fs)
    def ps: Time = Time(i, TimeUnit.ps)
    def ns: Time = Time(i, TimeUnit.ns)
    def us: Time = Time(i, TimeUnit.us)
    def ms: Time = Time(i, TimeUnit.ms)
    def s: Time = Time(i, TimeUnit.s)
  }

  extension (l: Long) {
    def fs: Time = Time(l, TimeUnit.fs)
    def ps: Time = Time(l, TimeUnit.ps)
    def ns: Time = Time(l, TimeUnit.ns)
    def us: Time = Time(l, TimeUnit.us)
    def ms: Time = Time(l, TimeUnit.ms)
    def s: Time = Time(l, TimeUnit.s)
  }

  def apply(value: Long, unit: TimeUnit): Time = {
    val t = new Time
    t.valueFs = value * math.pow(10, unit.exp).toLong
    t
  }
}

class Time extends Ordered[Time] {

  import Time.TimeUnit

  private[framework] var valueFs: Long = 0

  def +(that: Time): Time = {
    Time(valueFs + that.valueFs, TimeUnit.fs)
  }
  def -(that: Time): Time = {
    Time(valueFs - that.valueFs, TimeUnit.fs)
  }
  def *(that: Long): Time = {
    Time(valueFs * that, TimeUnit.fs)
  }
  def /(that: Time): Long = {
    valueFs / that.valueFs
  }
  def /(that: Long): Time = {
    Time(valueFs / that, TimeUnit.fs)
  }

  def toString(unit: TimeUnit): String = {
    val exp = unit.exp
    val v = valueFs / math.pow(10, exp)
    s"%.3f ${unit}".format(v)
  }

  private def findBestTimeUnit(t: Time, unit: TimeUnit): TimeUnit = {
    val v = valueFs / math.pow(10, unit.exp)
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
    valueFs / unit.valueFs
  }

  override def compare(that: Time): Int = {
    this.valueFs.compare(that.valueFs)
  }
}