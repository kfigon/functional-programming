package course.part4implicits

import org.scalatest.funspec.AnyFunSpec

class ImplSpec extends AnyFunSpec {
  // if compilation fails, implicit is a fallback to try to match types
  it("implicit conversions") {
    // how does this work? There's no -> method on string
    // there's an implicit class ArrowAssoc that'll be used when this is not found
    val pair = "foo" -> "bar"

    case class Person(name: String) {
      def greet = s"hi, my name is $name"
    }

    implicit def fromStringToPerson(str: String): Person = Person(str)

    // compiler looks for anything with greet method and tries to match it. Converts using anything implicit that matches with that method
    // in case of conflits it'll fail
    val greeting = "Peter".greet // fromStringToPerson("Peter").greet
    assert(greeting == "hi, my name is Peter")
  }

  // like default params, but for convenience - less passing around - like future executor
  it("implicit param") {
    def inc(x: Int)(implicit amount: Int) = x + amount
    implicit val defaultAmount: Int = 10

    assert(inc(1) == 11)
  }
}
