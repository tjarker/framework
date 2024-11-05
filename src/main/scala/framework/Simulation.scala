package framework

import scala.util.DynamicVariable

import framework.* 
import framework.types.*
import java.lang.ModuleLayer.Controller
import framework.Time.ns

object Simulation {

  case class SimulationContext(
    ctrl: SimController,
    timeUnit: Time,
    debug: Boolean
  )

  private val contextObject = DynamicVariable[Option[SimulationContext]](None)
  def ctx: SimulationContext = contextObject.value.getOrElse(throw new RuntimeException("No context found"))

  private val loggerObject = DynamicVariable[Option[Logger]](None)
  def logger: Logger = loggerObject.value.getOrElse(throw new RuntimeException("No logger found"))

  def apply[M <: Module](m: M, timeUnit: Time, debug: Boolean)(block: M => Unit): Unit = {

    loggerObject.withValue(Some(Logger(debug))) {
      val context = SimulationContext(SimController(m, timeUnit), timeUnit, debug)
      contextObject.withValue(Some(context)) {
        block(m)
        ctx.ctrl.sim.destroy()
      }
    }
    
  }

  inline def info(provider: String, msg: String): Unit = logger.info(s"sim.$provider", msg)
  inline def warning(provider: String, msg: String): Unit = logger.warning(s"sim.$provider", msg)
  inline def error(provider: String, msg: String): Unit = logger.error(s"sim.$provider", msg)

  def fork(name: String)(body: => Unit): Unit = ctx.ctrl.scheduler.addThread(name)(body)

  def peek[T <: Bits](p: Port[T]): BigInt = ctx.ctrl.peek(p)

  def poke[T <: Bits](p: Input[T], value: BigInt): Unit = ctx.ctrl.poke(p, value)

  def step(c: Input[Clock], steps: Int): Unit = ctx.ctrl.step(c, steps)

  def time: SimulationTime = ctx.ctrl.simTime
  
}
