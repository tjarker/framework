package framework

import Time.*

class SimulationTime(ticker: () => (AbsoluteTime => Unit)) extends AbsoluteTime {

    this.valueFs = 0

    def tick(t: Time): Unit = {
        ticker()((this + t).absolute)
    }

    private[framework] def inc(t: RelativeTime): Unit = {
        this.valueFs += t.valueFs
    }

    private[framework] def set(t: AbsoluteTime): Unit = {
        this.valueFs = t.valueFs
    }

}
