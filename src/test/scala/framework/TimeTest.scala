
package framework

import Time.*

import org.scalatest.flatspec.AnyFlatSpec

class TimeTest extends AnyFlatSpec {

    "Time" should "add" in {
        val t1 = 1.ns
        val t2 = 1.us
        val t3 = t1 + t2
        assert(t3 == 1001.ns)
    }

    "Time" should "subtract" in {
        val t1 = 1.ns
        val t2 = 1.us
        val t3 = t2 - t1
        assert(t3 == 999.ns)
    }

    "Time" should "format" in {
        assert(100001.ms.toString == "100.001 s")
        assert(100001.us.toString == "100.001 ms")
        assert(100001.ns.toString == "100.001 us")
        assert(100001.ps.toString == "100.001 ns")
        assert(100001.fs.toString == "100.001 ps")
    }

}