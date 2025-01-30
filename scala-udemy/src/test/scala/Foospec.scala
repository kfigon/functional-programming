import org.scalatest.funspec.AnyFunSpec

import scala.io.Source
import scala.language.postfixOps

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

  describe("overengineered aoc template") {
    trait Parser[T] {
      def parse(v: String): T
    }

    trait Solver[T, Out] {
      def solve(v: T): Out
    }

    def doIt[T, Out](v: String)(parser: Parser[T])(solver: Solver[T, Out]): Out = (parser.parse _ andThen solver.solve)(v)

    describe("d1") {
      val data =
        """123
          |456
          |798
          |111
          |222""".stripMargin

      case class Data(v: Int)

      val aParser = new Parser[List[Data]] {
        override def parse(v: String): List[Data] = v.linesIterator
          .map(_.toInt)
          .map(Data)
          .toList
      }

      def file(name: String): String = {
        val f = Source.fromFile(name)
        try {
          f.mkString
        } finally {
          f.close()
        }
      }

      val part1 = new Solver[List[Data], Int] {
        override def solve(v: List[Data]): Int = v.map(_.v).sum
      }

      def part2(threshold: Int) = new Solver[List[Data], Boolean] {
        override def solve(v: List[Data]): Boolean = v.map(_.v).exists(_ > threshold)
      }

      def part3(threshold: Int) = new Solver[List[Data], Either[String, Boolean]] {
        override def solve(v: List[Data]): Either[String, Boolean] = v.map(_.v).count(_ > threshold) match {
            case 0 => Right(true)
            case 1 => Right(false)
            case 2 => Left("too much!")
            case _ => Left("completely invalid")
          }
      }

      it("p1") {
        assert(doIt(data)(aParser)(part1) == 1710)
      }

      it("p2") {
        assert(!doIt(data)(aParser)(part2(1000)))
        assert(doIt(data)(aParser)(part2(700)))
      }

      it("p3 with messages") {
        val foo = (th: Int) => doIt(data)(aParser)(part3(th))

        assert(foo(1000) == Right(true))
        assert(foo(600) == Right(false))
        assert(foo(400) == Left("too much!"))
        assert(foo(5) == Left("completely invalid"))
      }
    }
  }
}
