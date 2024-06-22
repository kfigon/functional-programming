package course.part2functional

import org.scalatest.funspec.AnyFunSpec

//lazy delays evaluation till the usage
//it's also evaluated ONCE, useful when accepting a val by name (n: => Int)
//on collections - there's `withFilter` method, which filters using lazy vals under the hood
//for comprehensions on collections with guard is always lazy
class LazySpec extends AnyFunSpec {
  it("it throws") {
    assertThrows[RuntimeException]{
      val v = throw new RuntimeException
    }
  }
  it("this will not throw") {
    lazy val v = throw new RuntimeException
  }
}
