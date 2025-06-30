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
  def markRunning(t: Task[?]): Unit
  def markSleeping(t: Task[?]): Unit
  def retire(t: Task[?]): Unit
  def requestStepWakeup(t: Task[?], c: ClockPort, steps: Int): Unit
  def requestPoke(t: Task[?], p: Input[Bits], value: BigInt): Unit
  def requestPeek(t: Task[?], p: Port[Bits]): BigInt
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

  def getChildTasks: List[Task[?]]

  def markRunning(t: Task[?]): Unit = ctrl.markRunning(t)

  def markSleeping(t: Task[?]): Unit = ctrl.markSleeping(t)

  def retire(t: Task[?]): Unit = ctrl.retire(t)

  def logger: Logger = Logger(true)

  def poke(p: Input[Bits], value: BigInt): Unit = ctrl.requestPoke(Task.current, p, value)
  def peek(p: Port[Bits]): BigInt = ctrl.requestPeek(Task.current, p)

  def peekMonitor(p: Input[Bits]): BigInt = ctrl.requestPeekMonitor(Task.current, p)

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
