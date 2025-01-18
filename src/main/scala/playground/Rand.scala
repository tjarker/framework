package playground

import util.Random
import scala.reflect.Enum
import scala.deriving.Mirror

trait Rand[A] {
  def next: A
}

trait RandRanged[A] extends Rand[A] {
  def next: A
}

given RandRanged[Int] with {
  def next: Int = Random.between(0, 100)
}



given Rand[Int] with {
  def next: Int = Random.nextInt()
}


import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.ClassTag

class EnumValues[E](val values: IArray[E])

object EnumValues:

  inline given scalaEnum[E: ClassTag](using m: Mirror.SumOf[E]): EnumValues[E] =
    EnumValues(
      IArray.from(
        summonAll[Tuple.Map[m.MirroredElemTypes, ValueOf]].toList
          .asInstanceOf[List[ValueOf[E]]]
          .map(_.value)
      )
    )



given RandEnum[T <: Enum](using e: EnumValues[T]): Rand[T] with {
  def next: T = {
    val values = e.values
    values(Random.nextInt(values.length))
  }
}

object Rand {
  

  def apply[A](using r: Rand[A]): Rand[A] = r


}


@main def runRand: Unit = {
  val r = Rand[Int]
  println(r.next)

  enum Suit:
    case Hearts, Diamonds, Clubs, Spades

  val r2 = Rand[Suit]
  println(r2.next)
}


