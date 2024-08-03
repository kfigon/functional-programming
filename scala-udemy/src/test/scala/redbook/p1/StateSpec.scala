package redbook.p1

import org.scalatest.funspec.AnyFunSpec

import java.time.{LocalDateTime, ZoneOffset}

class StateSpec extends AnyFunSpec {
  describe("rng") {
    trait RNG {
      def int: (Int, RNG)
    }
    case class SimpleRng(seed: Long) extends RNG {
      override def int: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        val next = (newSeed >>> 16).asInstanceOf[Int]

        (next, SimpleRng(newSeed))
      }
    }

    it("foo") {
      val ts = () => LocalDateTime.now().toEpochSecond(ZoneOffset.UTC)
      var r: RNG = SimpleRng(ts())
      var res = List[Int]()

      for (_ <- 1 to 20) {
        val (v, next) = r.int
        res = v :: res
        r = next
      }

      println(res)
    }
}
