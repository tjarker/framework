package framework.simulation

import _root_.framework.types.*
import _root_.framework.coop.*
import _root_.framework.simulation.ModuleInterface.*

class CoopSimController(
  val dut: ModuleInterface,
  timeUnit: Time,
  debug: Boolean,
  wave: Option[String]
) {

  enum TaskStatus {
    case Running
    case WaitForStep
    case BlockedSend
    case BlockedRead
    case SelfBlocked
    case JoinBlocked(t: String)
    case WaitForMonitorRegion
  }


  import sys.process.*

  import java.nio.file.{Files, Paths, StandardOpenOption}
  import java.nio.charset.StandardCharsets
  import java.nio.file.Path

  import com.sun.jna.*

  val p = Paths.get(s"test/${dut.name}")
  HarnessGenerator.generate(dut, p)
  MakefileGenerator.generate(dut, p)

  Process("make clean_copies all", p.toFile).!!

  val libPath = s"${p.toAbsolutePath}/build/lib${dut.name}${MakefileGenerator.libExtension}"
  val libCopy = s"${p.toAbsolutePath}/build/lib${dut.name}_${java.time.Instant.now().toEpochMilli}${MakefileGenerator.libExtension}"
  Files.copy(Paths.get(libPath), Paths.get(libCopy))

  

  val opts = new java.util.HashMap[String, Int]()
  opts.put(Library.OPTION_OPEN_FLAGS, 2)
  val so = NativeLibrary.getInstance(libCopy, opts)

  val model =
    VerilatorInterface(
      so,
      dut,
      wave.getOrElse(null),
      timeUnit
    )

  val time = SimulationTime(null)

  val taskStatus = collection.mutable.Map[Task[?], TaskStatus]()
  val names = collection.mutable.Map[Thread, String]()

  val queue = InteractionQueue()


  def registerTask(t: Task[?], name: String): Unit

  def deregisterTask(t: Task[?]): Unit

  def poke(t: Task[?], p: Input[Bits], value: BigInt): Unit

  def peek(t: Task[?], p: Port[Bits]): BigInt

  def peekMonitor(t: Task[?], p: Input[Bits]): BigInt

  def peekReg(t: Task[?], r: Register): BigInt

  def step(t: Task[?], c: ClockPort, steps: Int): Unit




}

/*

- the sim controller should not provide the exact peek/poke/step methods but rather primitives such as:
  - 
 
*/