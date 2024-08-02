package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class EitherSpec extends AnyFunSpec {
  sealed trait Eithr[+E, +A] {
    def map[B](f: A => B): Eithr[E, B] = this match {
      case ELeft(v) => ELeft(v)
      case ARight(v) => ARight(f(v))
    }

    def flatmap[EE >: E, B](f: A => Eithr[EE, B]): Eithr[EE, B] = this match {
      case ELeft(v) => ELeft(v)
      case ARight(v) => f(v)
    }

    def orElse[EE >: E, B >: A](defaultV: => Eithr[EE, B]): Eithr[EE, B] = this match {
      case ELeft(_) => defaultV
      case ARight(v) => ARight(v)
    }

    def map2[EE >: E, B, C](v: => Eithr[EE, B])(f: (A, B) => C): Eithr[EE, C] = (this, v) match {
      case (ARight(a), ARight(other)) => ARight(f(a, other))
      case (ARight(_), ELeft(e)) => ELeft(e)
      case (ELeft(a), _) => ELeft(a)
    }
  }

  case class ELeft[+E](v: E) extends Eithr[E, Nothing]

  case class ARight[+A](v: A) extends Eithr[Nothing, A]


  describe("ex7") {
    val ok: Eithr[String, Int] = ARight(123)
    val bad: Eithr[String, Int] = ELeft("oops")

    it("map") {
      assert(ok.map(_ + 1) == ARight(124))
      assert(bad.map(_ + 1) == ELeft("oops"))
    }

    it("flatmap") {
      assert(ok.flatmap(v => ARight(v + 1)) == ARight(124))
      assert(bad.flatmap(v => ARight(v + 1)) == bad)
    }
    it("orElse") {
      assert(ok.orElse(ARight(-1)) == ARight(123))
      assert(bad.orElse(ARight(-1)) == ARight(-1))
    }

    it("map2") {
      val f: (Int, Int) => Int = _ + _
      assert(ok.map2(ok)(f) == ARight(246))
      assert(ok.map2(bad)(f) == ELeft("oops"))
      assert(bad.map2(bad)(f) == ELeft("oops"))
      assert(bad.map2(ok)(f) == ELeft("oops"))
    }
  }

  describe("ex8") {
    it("sequence") {
      def sequence[A, B](xs: List[Eithr[A, B]]): Eithr[A, List[B]] = {
        def run(xs: List[Eithr[A, B]], res: List[B]): Eithr[A, List[B]] = xs match {
          case Nil => ARight(res)
          case ::(ELeft(head), _) => ELeft(head)
          case ::(ARight(head), tl) => run(tl, res ++ List(head))
        }

        run(xs, Nil)
      }

      val exp: Eithr[String, List[Int]] = ARight(List(1, 2, 3))
      val in: List[Eithr[String, Int]] = List(ARight(1), ARight(2), ARight(3))
      val in2: List[Eithr[String, Int]] = List(ARight(1), ELeft("oops"), ARight(3))

      assert(sequence(in) == exp)
      assert(sequence(in2) == ELeft("oops"))
    }

    it("traverse") {
      def traverse[A, B, C](a: List[A])(fn: A => Eithr[B, C]): Eithr[B, List[C]] = {
        def run(a: List[A], out: List[C]): Eithr[B, List[C]] = a match {
          case Nil => ARight(out)
          case ::(head, tl) => fn(head) match {
            case ELeft(v) => ELeft(v)
            case ARight(v) => run(tl, out ++ List(v))
          }
        }

        run(a, Nil)
      }

      val in = List("1", "2", "3")
      val in2 = List("1", "asd", "3")
      val f: String => Eithr[String, Int] = v => try {
        ARight(v.toInt)
      } catch {
        case _: Exception => ELeft("invalid str")
      }

      assert(traverse(in)(f) == ARight(List(1, 2, 3)))
      assert(traverse(in2)(f) == ELeft("invalid str"))
    }
  }
}