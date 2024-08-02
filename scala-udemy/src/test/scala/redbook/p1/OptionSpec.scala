package redbook.p1

import org.scalatest.funspec.AnyFunSpec

import scala.util.Try

class OptionSpec extends AnyFunSpec {
  def lift[A,B](fn: A => B): Option[A] => Option[B] =
    optA => optA.map(fn)

  it("lift") {
    val toInt = (v: String) => v.toInt
    val lifted = lift(toInt)

    assert(lifted(None) == None)
    assert(lifted(Some("123")) == Some(123))
  }

  it("ex3 map2") {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
      case (Some(av), Some(bv)) => Some(f(av,bv))
      case _ => None
    }

    def map2usingFor[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = for {
      av <- a
      bv <- b
    } yield f(av,bv)

    val f: (Int, Int) => Int = (a,b) => a + b
    assert(map2(None, None)(f) == None)
    assert(map2usingFor(None, None)(f) == None)

    assert(map2(Some(1), Some(2))(f) == Some(3))
    assert(map2usingFor(Some(1), Some(2))(f) == Some(3))
  }

  it("ex5 sequence") {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      @scala.annotation.tailrec
      def run(a: List[Option[A]], result: List[A]): Option[List[A]] = a match {
        case Nil => Some(result)
        case ::(Some(head), tl) => run(tl, result ++ List(head)) // append to the end, this is foldl, so reverse order
        case _ => None
      }
      run(a, Nil)
    }

    assert(sequence(List(Some(1), Some(2), None, Some(3))) == None)
    assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(List(1,2,3,4)))
    assert(sequence(List()) == Some(List()))
    assert(sequence(List(None)) == None)
  }

  describe("ex6 traverse") {
  // sequence and map in one step

    def traverse[A,B](a: List[A])(fn: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case ::(head, tl) => for {
        v <- fn(head)
        rest <- traverse(tl)(fn)
      } yield v :: rest // both present, cons them
    }

    val f: String => Option[Int] = v => Try(v.toInt).toOption

    val l1 = List("1", "2", "3")
    val l2 = List("1", "asdf", "3")
    val l3 = List()

    it("trav") {
      assert(traverse(l1)(f) == Some(List(1,2,3)))
      assert(traverse(l2)(f) == None)
      assert(traverse(l3)(f) == Some(List()))
    }

    it("sequence in terms of traverse") {
      def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(v => v)

      assert(sequence(List(Some(1), Some(2), None, Some(3))) == None)
      assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(List(1,2,3,4)))
      assert(sequence(List()) == Some(List()))
      assert(sequence(List(None)) == None)
    }
  }
}