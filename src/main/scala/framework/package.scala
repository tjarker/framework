



import gears.async.SyncChannel
import gears.async.Async

package object framework {

    export Simulation.{fork, forkComp}
    export types.PokeHandler.given
    export types.PeekHandler.given
    export Util.*

    export gears.async.Async
    
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
}

