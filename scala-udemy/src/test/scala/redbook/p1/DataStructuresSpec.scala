package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class DataStructuresSpec extends AnyFunSpec {
  sealed trait AList[+A]
  case object Nil extends AList[Nothing]
  case class Cons[+A](head: A, tail: AList[A]) extends AList[A]

  object AList {
    def apply[A](as: A*): AList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, AList(as.tail: _*))


    def tail[A](xs: AList[A]): AList[A] = xs match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    def setHead[A](x:A, xs: AList[A]): AList[A] = Cons(x, xs)

    def drop[A](num: Int, xs: AList[A]): AList[A] = {
      @scala.annotation.tailrec
      def run(xs: AList[A], i: Int): AList[A] =
        if(i >= num) xs
        else run(tail(xs), i+1)

      run(xs, 0)
    }

    @scala.annotation.tailrec
    def dropWhile[A](xs: AList[A], fn: A => Boolean): AList[A] = xs match {
      case Nil => Nil
      case Cons(head, tail) if fn(head) => dropWhile(tail, fn)
      case x => x
    }

    def dropWhile2[A](xs: AList[A])(fn: A => Boolean): AList[A] = dropWhile(xs, fn) // better type inference

    def append[A](as: AList[A], bs: AList[A]): AList[A] = as match {
      case Nil => bs
      case Cons(head, tail) => Cons(head, append(tail, bs))
    }

    def init[A](xs: AList[A]): AList[A] = xs match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail)) // we need to traverse all the way down, can't do that in constant time
    }

    def foldr[A,B](xs: AList[A], zero: B)(fn: (A,B) => B): B = xs match {
      case Nil => zero
      case Cons(head, tail) => fn(head, foldr(tail, zero)(fn))
    }

    @scala.annotation.tailrec
    def foldl[A,B](xs: AList[A], zero: B)(fn: (A,B) => B): B = xs match { // tailrec, but reverses the order
      case Nil => zero
      case Cons(head, tail) => foldl(tail, fn(head, zero))(fn)
    }
  }

  it("ex1 pattern match") {
    val x = AList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case _ => 101
    }

    assert(x == 3)
  }

  it("ex2 tail") {
    assert(AList.tail(AList(1,2,3,4,5)) == AList(2,3,4,5))
    assert(AList.tail(AList()) == AList())
    assert(AList.tail(Nil) == Nil)
    assert(AList.tail(Cons(1, Cons(2, Cons(3, Nil)))) == AList(2,3))
  }

  it("ex3 setHead") {
    assert(AList.setHead(1, AList(2,3,4)) == AList(1,2,3,4))
    assert(AList.setHead(1, Nil) == AList(1))
    assert(AList.setHead(1, Cons(2,Cons(3, Nil))) == AList(1,2,3))
  }
  it("ex4 drop") {
    assert(AList.drop(2, AList(1,2,3,4,5)) == AList(3,4,5))
    assert(AList.drop(1, AList(1,2,3)) == AList(2,3))
    assert(AList.drop(3, AList(1,2)) == AList())
    assert(AList.drop(3, AList(1,2)) == Nil)
    assert(AList.drop(3, Nil) == AList())
  }

  it("ex5 dropWhile") {
//    type inference is bad in such callbacks. curried functions provide better inference
    assert(AList.dropWhile(AList(2,2,4,6,3,4,5), (v: Int) => v % 2 == 0) == AList(3,4,5))
    assert(AList.dropWhile(AList(1,2), (v: Int) => v % 2 == 0) == AList(1,2))
    assert(AList.dropWhile(AList("foo", "foo", "bar", "foo"), (v: String) => v == "foo") == AList("bar", "foo"))
    assert(AList.dropWhile(AList("foo", "foo", "foo"), (v: String) => v == "foo") == AList())

    // better inference with currying, no need to type annotate
    assert(AList.dropWhile2(AList(2,2,4,6,3,4,5))(_ % 2 == 0) == AList(3,4,5))
  }

  it("append") {
    assert(AList.append(AList(), AList()) == AList())
    assert(AList.append(AList(1), AList()) == AList(1))
    assert(AList.append(AList(), AList(1)) == AList(1))
    assert(AList.append(AList(1,2), AList(3)) == AList(1,2,3))
    assert(AList.append(AList(0), AList(1,2,3)) == AList(0,1,2,3))
  }

  it("ex6 init") {
    assert(AList.init(AList(1,2,3,4)) == AList(1,2,3))
    assert(AList.init(AList()) == AList())
    assert(AList.init(AList(1)) == AList())
  }

  describe("foldr") {
    val l = AList(1, 2, 3, 4, 5)

    it("ex11 simple reduce") {
      val sum = AList.foldr(l, 0)(_ + _)
      val product = AList.foldr(l, 1)(_ * _)

      assert(sum == 15)
      assert(product == 120)
    }

    it("ex14 append") {
      val out = AList.foldr(AList(1,2,3), AList(4,5,6))(Cons(_, _))
      assert(out == AList(1,2,3,4,5,6))
    }

    it("ex8") {
      val out = AList.foldr(AList(1,2,3), AList[Int]())(Cons(_, _))
      assert(out == AList(1,2,3))
    }

    it("ex9 len") {
      assert(AList.foldr(l,0)((_,acc) => 1 + acc) == 5)
    }
  }

  describe("foldl") {
    val l = AList(1, 2, 3, 4, 5)

    it("ex11 simple reduce") {
      val sum = AList.foldl(l, 0)(_ + _)
      val product = AList.foldl(l, 1)(_ * _)

      assert(sum == 15)
      assert(product == 120)
    }

    it("ex14 append") {
      val out = AList.foldl(AList(1,2,3), AList(4,5,6))(Cons(_, _))
      assert(out == AList(3,2,1,4,5,6))
    }

    it("reverse") {
      val out = AList.foldl(AList(1,2,3), AList[Int]())(Cons(_, _))
      assert(out == AList(3,2,1))
    }

    it("ex9 len") {
      assert(AList.foldl(l,0)((_,acc) => 1 + acc) == 5)
    }
  }


//  it("ex") {fail}
}
