package framwork.types

trait HardwareType[T <: Bits] {
    def width: Width
    def materialize: T

    override def toString(): String = {
        s"Type(${width})"
    }
}
