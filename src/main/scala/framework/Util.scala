package framework

object Util {
  def log2ceil(x: BigInt): Int = (math.log(x.toDouble) / math.log(2)).ceil.toInt


  extension [T](s: Array[T]) {
    def oneOf: T = s.apply(scala.util.Random.nextInt(s.size))
  }
}
