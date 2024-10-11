package framework

import framework.Time

class SimulationTime(tick: () => (Time => Unit)) extends Time {

    this.valueFs = 0

    def tick(t: Time): Unit = {
        tick()(t)
    }

    def inc(t: Time): Unit = {
        this.valueFs += t.valueFs
    }

}
