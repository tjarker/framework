
import language.future

package object framework {
  
    def simulate[M <: Module](m: M): M = {
        val ctrl = new SimController(m)
        m
    }

}
