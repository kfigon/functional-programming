package course.part4implicits

import org.scalatest.funspec.AnyFunSpec

//implicit classes to enrich other classes

//type conversions can be dangerous. typeclasses are more safe
class EnrichmentsSpec extends AnyFunSpec {

//  implicit class BetterInt(val v: Int) extends AnyVal { // memory optimization
  // compiler goes just one step, it does not go recursively
  implicit class BetterInt(v: Int) {
    def isEven: Boolean = v % 2 == 0
    def pow2: Int = v*v
  }

//  type enrichment
  it("enrich int") {
    // this is why (1 to 10) or 2.seconds works
    assert(42.isEven) // compiler rewrites it as new BetterInt(42).isEven
    assert(!43.isEven)
    assert(3.pow2 == 9)
  }
}
