package course.part2functional

import org.scalatest.funspec.AnyFunSpec

class PartialsSpec extends AnyFunSpec {
  // when we just want a function to accept a subset of all values. It'll throw if not matched

  describe("partial") {
    val verboseFun = (x: Int) => x match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "threee"
      case n => throw new Exception(s"unsupported val $n")
    }

    val patternMatched = (x: Int) => x match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "threee"
    }

    // exactly the same as previous ones
    val properOne: PartialFunction[Int, String] = {
      case 1 => "one"
      case 2 => "two"
      case 3 => "threee"
    }

    List(
      (1, "one"),
      (2, "two"),
      (3, "threee"),
    ).foreach(t => it(t._2) {
      val in = t._1
      val exp = t._2
      assert(verboseFun(in) == exp)
      assert(patternMatched(in) == exp)
      assert(properOne(in) == exp)
    })

    describe("utils") {
      it("defined") {
        assert(properOne.isDefinedAt(3))
        assert(!properOne.isDefinedAt(4))
      }

      it("lift") {
        val lifted = properOne.lift
        assert(lifted(2) == Some("two"))
        assert(lifted(5) == None)
      }

      it("orElse") {
        val foo: PartialFunction[Int, String] = properOne.orElse({
          case _ => "unknown"
        })

        assert(foo(1) == "one")
        assert(foo(11) == "unknown")
      }
    }
  }
}
