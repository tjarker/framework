import framework.simulation.ModuleInterface




import gears.async.SyncChannel
import gears.async.Async

package object framework {

    export simulation.Simulation.{fork, forkComp}
    export types.PokeHandler.given
    export types.PeekHandler.given
    export Util.*
    export simulation.Time.*
    export simulation.Simulation
    export simulation.Sim
    export simulation.ModuleInterface

    export framework.types.Types.*
    export framework.types.Input
    export framework.types.Output
    export framework.types.Bool
    export framework.types.UInt
    export framework.types.SInt
    export framework.types.Struct
    export framework.types.ClockPort
    export framework.types.ResetPort


    export gears.async.Async

    import simulation.*
    
    def forever(body: => Unit): Unit = {
        while (true) {
            body
        }
    }


    extension (r: ModuleInterface.Register) {
        def peekReg(using Sim, Async): BigInt = {
            summon[Sim].peekReg(r)
        }
    }


    extension [T <: Transaction](s: Seq[T]) {
        def toSequence(using Hierarchy): Sequence[T, T] = {
            SequenceComposition.ScalaSeq[T](s).asInstanceOf[Sequence[T, T]]       
        }
    }


    import scala.reflect.ClassTag
    inline def param[T: ClassTag](using Hierarchy): T = {
        Config
        .tryGet(macros.Naming.enclosingTermName)
        .getOrElse(throw new Exception(s"Object ${summon[Hierarchy].name} expects parameter ${macros.Naming.enclosingTermName}"))
        .asInstanceOf[T]
    }

    inline def create[T](name: String, c: Hierarchy ?=> T, conf: Map[Any,Any])(using Hierarchy): T = {
        val newHierarchy = Factory.setupEnvironment(summon[Hierarchy], name, conf, Map.empty)
        val obj = c(using newHierarchy)
        obj match {
            case c: Component => summon[Hierarchy].getComponent.get.children += c
            case _ => ()
        }
        obj
    }

    inline def create[T](name: String, c: Hierarchy ?=> T)(using Hierarchy): T = {
        create(name, c, Map.empty)
    }

    inline def create[T](c: Hierarchy ?=> T)(using Hierarchy): T = {
        create(macros.Naming.enclosingTermName, c, Map.empty)
    }

    inline def create[T](c: Hierarchy ?=> T, conf: Map[Any,Any])(using Hierarchy): T = {
        create(macros.Naming.enclosingTermName, c, conf)
    }


}

