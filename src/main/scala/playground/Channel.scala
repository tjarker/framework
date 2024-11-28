package playground

import java.util.concurrent.locks.{ReentrantLock, Condition}

class SynchronizedChannel[T] {
  private var message: Option[T] = None
  private val lock = ReentrantLock(true) // Fair lock to ensure fairness
  private val hasMessage = lock.newCondition()
  private val hasSpace = lock.newCondition()

  // Sender puts a message into the channel
  def send(msg: T): Unit = {
    lock.lock()
    try {
      while (message.isDefined) {
        hasSpace.await() // Wait if the channel is full
      }
      message = Some(msg)
      println(s"Message sent: $msg")
      hasMessage.signal() // Notify receivers
    } finally {
      lock.unlock()
    }
  }

  // Receiver retrieves a message from the channel
  def receive(): T = {
    lock.lock()
    try {
      while (message.isEmpty) {
        hasMessage.await() // Wait if the channel is empty
      }
      val msg = message.get
      message = None
      println(s"Message received: $msg")
      hasSpace.signal() // Notify senders
      msg
    } finally {
      lock.unlock()
    }
  }
}

@main def runSynchronizedChannel(): Unit = {
  val channel = SynchronizedChannel[String]()

  val sender = for (id <- 0 to 3) yield Thread.startVirtualThread(() => {
    for (i <- 0 until 5) {
      val msg = s"$id: Message $i"
      println(s"$id: Trying to send: $msg")
      channel.send(msg)
      Thread.sleep(500) // Simulate some work
    }
  })

  val receiver = Thread.startVirtualThread(() => {
    for (_ <- 0 until 20) { // Total messages = 4 senders x 5 messages
      val msg = channel.receive()
      println(s"Receiver got: $msg")
    }
  })

  sender.foreach(_.join())
  receiver.join()
}
