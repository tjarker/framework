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
