package framework

import Time.AbsoluteTime
import types.*

enum Interaction(val time: AbsoluteTime) {
  case Drive(t: AbsoluteTime, p: Input[Bits], value: BigInt)
      extends Interaction(t)
  case PosEdge(t: AbsoluteTime, p: ClockPort) extends Interaction(t)
  case NegEdge(t: AbsoluteTime, p: ClockPort) extends Interaction(t)
  case Release(t: AbsoluteTime, thread: Thread) extends Interaction(t)
}

class InteractionQueue {

  val queue =
    collection.mutable.PriorityQueue.empty[Interaction](Ordering.by { i =>
      (
        -i.time.fs,
        i match
          case _: Interaction.Drive   => 2
          case _: Interaction.Release => 0
          case _                      => 1
      )
    })

  def add(i: Interaction): Unit = queue.enqueue(i)

  def nextInteractionTime: AbsoluteTime = queue.head.time

  def getInteractionsForThisTime: List[Interaction] = {
    val time = nextInteractionTime
    val interactions = collection.mutable.ListBuffer.empty[Interaction]
    while (queue.nonEmpty && queue.head.time == time) {
      interactions += queue.dequeue()
    }
    interactions.toList
  }

  def pop(): Interaction = queue.dequeue()

}
