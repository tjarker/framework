package framework

import scala.util.Random

import framework.types.Width

object Rand {


  def uint(w: Width): BigInt = {
    BigInt(w.toInt, Random)
  }

  def between (min: Int, max: Int): Int = {
    Random.between(min, max)
  }

  def oneof[T](choices: Seq[T]): T = {
    choices(Random.nextInt(choices.size))
  }
  def oneof[T](choices: Array[T]): T = {
    choices(Random.nextInt(choices.size))
  }

  def bool(): Boolean = {
    Random.nextBoolean()
  }

}