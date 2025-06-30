package framework.coop

trait Scheduler {
  def currentTask: Task[?]
  def launchTask[T](r: => T): Task[T]
  def suspendCurrentTask(): Unit
  def scheduleTask(task: Task[?]): Unit
  def cancelTask(task: Task[?]): Unit
  def runBlocking[T](block: => T): T
}

object Scheduler {
  import scala.util.DynamicVariable

  // Check if the Continuation API is available
  val hasContinuations: Boolean =
    try {
      new jdk.internal.vm.ContinuationScope("test")
      true
    } catch {
      case _: NoClassDefFoundError => false
      case _: IllegalAccessError   => false
      case _: Throwable            => false
    }

  // Check if the Virtual Threads API is available
  val hasVirtualThreads: Boolean =
    try {
      val thread = Thread.ofVirtual()
      true
    } catch {
      case _: NoSuchMethodError => false
      case _: Throwable         => false
    }

  private val schedulerFactory: () => Scheduler = {
    if (hasContinuations) { () =>
      println("Using Continuation Scheduler")
      new ContinuationScheduler()
    } else if (hasVirtualThreads) { () =>
      println("Using Virtual Thread Scheduler")
      new VirtualThreadScheduler()
    } else { () =>
      println("Using Platform Thread Scheduler")
      new PlatformThreadScheduler()
    }
  }

  def configureVirtualThreadScheduler(): Unit = {
    System.setProperty("jdk.virtualThreadScheduler.parallelism", "1")
    System.setProperty("jdk.virtualThreadScheduler.maxPoolSize", "1")
    System.setProperty("jdk.virtualThreadScheduler.minRunnable", "1")
  }

  private val currentScheduler = new DynamicVariable[Scheduler](null)

  private[coop] def apply(): Scheduler = schedulerFactory()

  def launchTask[T](r: => T): Task[T] =
    current.launchTask(r)

  private[coop] def scheduleTask(task: Task[?]): Unit =
    current.scheduleTask(task)

  private[coop] def suspendCurrentTask(): Unit =
    current.suspendCurrentTask()

  private[coop] def cancelTask(task: Task[?]): Unit =
    current.cancelTask(task)

  private[coop] def current: Scheduler = {
    val s = currentScheduler.value
    if (s == null) {
      throw new IllegalStateException("No scheduler is currently set")
    }
    s
  }

  def blocking[T](block: => T): T = {
    val newScheduler = schedulerFactory()
    currentScheduler.withValue(newScheduler) {
      newScheduler.runBlocking(block)
    }
  }

  private[coop] def blockingWithScheduler[T](
      s: Scheduler
  )(block: => T): T = {
    currentScheduler.withValue(s) {
      s.runBlocking(block)
    }
  }

}
