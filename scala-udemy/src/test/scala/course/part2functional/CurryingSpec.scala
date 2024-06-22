package course.part2functional

import org.scalatest.funspec.AnyFunSpec

class CurryingSpec extends AnyFunSpec {
  it("basic currying") {
    def bad(v: Int): Int => Int = a => v + a

    // just like in haskell:
    def better: Int => Int => Int = a => v => a + v
    def best(x: Int)(y: Int): Int = x + y

    assert(bad(4)(3) == 7)
    assert(better(4)(3) == 7)
    assert(best(4)(3) == 7)

    // partially applied function
    val add3 = better(3)
    assert(add3(4) == 7)

    val add3_2 = best(3)(_) // for defs we need that (_). Transforming method to function - lifting or ETA expansion
//    val add3_3: Int => Int = best(3) // alternatively this works
    assert(add3_2(4) == 7)
  }

  it("currying exercise - as many partial applications syntax as possible") {
    val adderLambda = (x: Int, y: Int) => x + y
    def simpleAddMethod(x: Int, y: Int): Int = x + y
    def curriedMethod(x: Int)(y: Int): Int = x + y

    val add1 = (x: Int) => adderLambda(7,x)
    val add2: Int => Int = x => adderLambda(7,x)
    val add3 = curriedMethod(7) _
    val add4 = adderLambda.curried(7)
    val add5 = simpleAddMethod(7, _)
    val add6 = adderLambda(7, _)
  }

  it("underscores") {
    def concatenate(a: String, b: String, c: String): String = a + b + c

    val foo = concatenate("1",_,"3") // we can have more, each underscore is next param
    assert(foo("2") == "123")
  }

  it("format list") {
    def formatter(format: String)(v: Double): String = format.format(v)
    val someFormatter = formatter("%8.6f")(_) // lift it

    val r = List(1.0f,2.0,3.0).map(someFormatter)
    assert(r == List("1.000000", "2.000000", "3.000000"))
  }
}
