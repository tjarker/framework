
import language.future

package object framework {
  
    def simulate[M <: Module](m: M, timeUnit: Time): M = {
        val ctrl = new SimController(m, timeUnit)
        m
    }

}
