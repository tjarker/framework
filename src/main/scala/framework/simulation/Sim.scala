package framework.simulation

import _root_.framework.coop.Task
import _root_.framework.types.*
import _root_.framework.ModuleInterface.Register
import _root_.framework.Logger

enum BlockReason {
  case Join(t: Task[?])
  case WaitForStep(c: ClockPort, steps: Int)
  case WaitForMonitorRegion
  case BlockedSend
  case BlockedReceive
}

trait SimControl {

  def time: SimulationTime
  def dut: ModuleInterface
  def registerTask(t: Task[?], name: String): Unit
  def markRunning(t: Task[?]): Unit
  def markSleeping(t: Task[?], reason: BlockReason): Unit
  def retire(t: Task[?]): Unit
  def requestStepWakeup(t: Task[?], c: ClockPort, steps: Int): Unit
  def requestPoke(t: Task[?], p: Input[Bits], value: BigInt): Unit
  def requestPeek(t: Task[?], p: Port[Bits]): BigInt
  def isInMonitorRegion(): Boolean
  def requestMonitorWakeup(t: Task[?]): Unit
  def requestPeekMonitor(t: Task[?], p: Input[Bits]): BigInt
  def requestPeekReg(t: Task[?], r: Register): BigInt
  def finish(t: Task[?]): Unit
  def abort(t: Task[?], e: Throwable): Unit

}

trait Sim {

  def ctrl: SimControl

  def currentClock: ClockPort

  def withClock(c: ClockPort): Sim

  def hierarchicalTaskName: String

  def addChildTask(f: Task[?]): Unit

  def getChildTasks: Seq[Task[?]]

  def registerTask(t: Task[?], name: String): Unit = ctrl.registerTask(t, name)

  def markRunning(t: Task[?]): Unit = ctrl.markRunning(t)

  def markSleeping(t: Task[?], reason: BlockReason): Unit = ctrl.markSleeping(t, reason)

  def retire(t: Task[?]): Unit = ctrl.retire(t)

  def logger: Logger = Logger(true)

  def poke(p: Input[Bits], value: BigInt): Unit =
    ctrl.requestPoke(Task.current, p, value)
  def peek(p: Port[Bits]): BigInt = ctrl.requestPeek(Task.current, p)

  def peekMonitor(p: Input[Bits]): BigInt = {
    if (!ctrl.isInMonitorRegion()) {
      ctrl.requestMonitorWakeup(Task.current)
      Task.suspendCurrent()
    }
    ctrl.requestPeekMonitor(Task.current, p)
  }

  def peekReg(r: Register): BigInt = ctrl.requestPeekReg(Task.current, r)

  def step(c: ClockPort, steps: Int): Unit = {
    ctrl.requestStepWakeup(Task.current, c, steps)
    Task.suspendCurrent()
  }

  def step(steps: Int): Unit = step(currentClock, steps)

  def finish(): Unit = ctrl.finish(Task.current)

  def abort(e: Throwable): Unit = ctrl.abort(Task.current, e)

  def time: SimulationTime = ctrl.time

}
