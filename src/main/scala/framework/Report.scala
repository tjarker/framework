package framework

object Logger {

  import Console.*
  import macros.FileContext

  inline def info(provider: String, msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    val framed = "\u001b[38;5;11m"
    Console.println(
      s"[info] [${YELLOW}$provider${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.println(s"[info] [${YELLOW}$provider${RESET}] ${line}")
    }
  }
  inline def info(provider: String, time: Time, msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    val framed = "\u001b[38;5;11m"
    Console.println(
      s"[info] [${YELLOW}$provider${RESET} @ ${MAGENTA}$time${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.println(s"[info] [${YELLOW}$provider${RESET}]@$time ${line}")
    }
  }
  inline def info(msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    Console.println(
      s"[info] ${lines.head}"
    )
    lines.tail.foreach { line =>
      Console.println(s"[info] ${line}")
    }
  }

  inline def warning(provider: String, msg: String): Unit = Logger.synchronized {
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

  inline def warning(msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    Console.err.println(
      s"[${YELLOW}warn${RESET}] ${lines.head}"
    )
    lines.tail.foreach { line =>
      Console.err.println(
        s"[${YELLOW}warn${RESET}] ${line}"
      )
    }
  }
  inline def error(provider: String, msg: String): Unit = Logger.synchronized {
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
  inline def error(msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    Console.err.println(
      s"[${RED}error${RESET}] ${lines.head}"
    )
    lines.tail.foreach { line =>
      Console.err.println(
        s"[${RED}error${RESET}] ${line}"
      )
    }
  }

  inline def success(provider: String, msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    Console.println(
      s"[${GREEN}success${RESET}] [${YELLOW}$provider${RESET}] ${lines.head} (${BLUE}${FileContext()}${RESET})"
    )
    lines.tail.foreach { line =>
      Console.println(
        s"[${GREEN}success${RESET}] [${YELLOW}$provider${RESET}] ${line}"
      )
    }
  }
  inline def success(msg: String): Unit = Logger.synchronized {
    val lines = msg.split("\n")
    Console.println(
      s"[${GREEN}success${RESET}] ${lines.head}"
    )
    lines.tail.foreach { line =>
      Console.println(
        s"[${GREEN}success${RESET}] ${line}"
      )
    }
  }

}

class Logger(enable: Boolean) {
  inline def info(provider: String, msg: String): Unit = if (enable) {
    Logger.info(provider, msg)  
  }
  inline def warning(provider: String, msg: String): Unit = if (enable) {
    Logger.warning(provider, msg)
  }
  inline def error(provider: String, msg: String): Unit = if (enable) {
    Logger.error(provider, msg)
  }
  inline def success(provider: String, msg: String): Unit = if (enable) {
    Logger.success(provider, msg)
  }
}


class ComponentLogger(name: String) {
  val logger = Logger(true)
  def info(msg: String): Unit = logger.info(name, msg)
  def warning(msg: String): Unit = logger.warning(name, msg)
  def error(msg: String): Unit = logger.error(name, msg)
  def success(msg: String): Unit = logger.success(name, msg)
}


trait Reportable {
  inline def info(msg: Any)(using s: Sim = null): Unit = if(s != null) {
    Logger.info(this.toString(), summon[Sim].time, msg.toString()) 
  } else {
    Logger.info(this.toString(), msg.toString())
  }
  inline def warning(msg: String): Unit = Logger.warning(this.toString(), msg)
  inline def error(msg: String): Unit = Logger.error(this.toString(), msg)
  inline def success(msg: String): Unit = Logger.success(this.toString(), msg)
}