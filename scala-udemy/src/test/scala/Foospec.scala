import org.scalatest.funspec.AnyFunSpec

class Foospec extends AnyFunSpec {
  it("implicit conversion") {
    case class Data(v: Int)

    implicit def convertIntToData(v: Int): Data = Data(v+100)

    val v: Data = 1 // int changed to Data with some processing
    assert(v == Data(101))
  }

  it("extending existing types") {
    implicit class BetterInt(left: Int) {
      def isFoobar(other: Int): Boolean = (left + 1) == other
    }

    assert(123.isFoobar(124))
  }

  it("extending existing types - generic") {
    implicit class BetterFoo[T](left: T) {
      def ====(other: T) = if(left != other) throw new Exception(s"$left != $other")
    }

    assertThrows[Exception](123 ==== 4567)
  }

  it("typeclasses - polymorphism with existing types") {
    trait Serialize[T] {
      def str(v: T): String
    }

    implicit object IntSerializer extends Serialize[Int] {
      override def str(v: Int): String = s"int$v"
    }

//    there can be multiple implementations for the same type, just import the right one
//    implicit object AlternativeIntSerializer extends Serialize[Int] {
//      override def str(v: Int): String = s"foobarInt${v}"
//    }

    implicit object BoolSerializer extends Serialize[Boolean] {
      override def str(v: Boolean): String = s"bool$v"
    }

    // or as a variable
    implicit val stringSerializer = new Serialize[String] {
      override def str(v: String): String = s"str_$v"
    }

    def executor[T](v: T)(implicit serializer: Serialize[T]): String = serializer.str(v)

    assert(executor(123) == "int123")
    assert(executor(true) == "booltrue")
    assert(executor("foobar") == "str_foobar")
  }
}
