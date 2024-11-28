



package object framework {

    export Simulation.fork
    export types.PokeHandler.given
    export types.PeekHandler.given
    export Util.*

    inline def info(msg: String)(using Sim): Unit = summon[Sim].logger.info("user", msg)
    inline def error(msg: String)(using Sim): Unit = summon[Sim].logger.error("user", msg)
    inline def warning(msg: String)(using Sim): Unit = summon[Sim].logger.warning("user", msg)

}
