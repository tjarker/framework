package framework

class Logger(enable: Boolean) {

  import Console.*
  import macros.FileContext

  inline def info(provider: String, msg: String): Unit = if (enable) {
    val lines = msg.split("\n")
    val framed = "\u001b[38;5;11m"
    Console.println(
      s"[info] [${YELLOW}$provider${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.println(s"[info] [${YELLOW}$provider${RESET}] ${line}")
    }
  }
  inline def warning(provider: String, msg: String): Unit = if (enable) {
    val lines = msg.split("\n")
    Console.err.println(
      s"[${YELLOW}warn${RESET}] [${YELLOW}$provider${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.err.println(
        s"[${YELLOW}warn${RESET}] [${YELLOW}$provider${RESET}] ${line}"
      )
    }
  }
  inline def error(provider: String, msg: String): Unit = if (enable) {
    val lines = msg.split("\n")
    Console.err.println(
      s"[${RED}error${RESET}] [${YELLOW}$provider${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.err.println(
        s"[${RED}error${RESET}] [${YELLOW}$provider${RESET}] ${line}"
      )
    }
  }

}

object Logger extends Logger(true)


class ComponentLogger(name: String) {
  val logger = Logger(true)
  def info(msg: String): Unit = logger.info(name, msg)
  def warning(msg: String): Unit = logger.warning(name, msg)
  def error(msg: String): Unit = logger.error(name, msg)
}


trait Reportable {
  inline def info(msg: String): Unit = Logger.info(this.toString(), msg)
  inline def warning(msg: String): Unit = Logger.warning(this.toString(), msg)
  inline def error(msg: String): Unit = Logger.error(this.toString(), msg)
}