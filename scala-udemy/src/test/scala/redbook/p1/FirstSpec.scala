package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class FirstSpec extends AnyFunSpec {
  it("ex1 fibo") {
    def fibo(n: Int): Int = {

      @annotation.tailrec
      def run(i: Int, prev: Int, current: Int): Int =
        if (n == 0) 0
        else if(i >= n) current
        else run(i+1, current, prev + current)

      run(1, 0, 1)
    }

    assert((0 to 19).map(fibo).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181))
  }

  it("ex2 isSorted") {
    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
      @scala.annotation.tailrec
      def run(as: Array[A], idx: Int): Boolean = {
        if (idx + 1 >= as.length) true
        else if (gt(as(idx), as(idx + 1))) false
        else run(as, idx + 1)
      }

      run(as, 0)
    }

    val gt = (a: Int,b: Int) => a > b

    assert(isSorted(Array(1,2,3,3,4,5), gt))
    assert(!isSorted(Array(1,2,2,5,4,3), gt))
  }

  it("ex3 curry") {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      (a: A) => (b: B) => f(a, b)

    val even = (a: Int, b: String) => (a + b.length) % 2 == 0
    val asd = curry(even)

    assert(asd(1)("foo"))
    assert(!asd(2)("foo"))
    assert(!asd(1)("foox"))
  }

  it("ex4 uncurry") {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a: A, b: B) => f(a)(b)

    val even = (a: Int) => (b: String) => (a + b.length) % 2 == 0
    val asd = uncurry(even)

    assert(asd(1, "foo"))
    assert(!asd(2, "foo"))
    assert(!asd(1, "foox"))
  }

  it("compose vs andthen") {
    val even = (v: Int) => v % 2 == 0
    val strLen = (v: String) => v.length

    // both are even(strLen)
    val composed = even compose strLen
    val andThened = strLen andThen even // order is more natural here

    assert(composed("xfoo"))
    assert(!composed("foo"))

    assert(andThened("xfoo"))
    assert(!andThened("foo"))
  }

  it("ex5 compose") {
    def compose[A,B,C](f: B => C, g: A => B): A => C =
      (a: A) => f(g(a))

    val asd = compose[String, Int, Boolean](
      i => i % 2 == 0,
      _.length
    )

    assert(asd("xfoo"))
    assert(!asd("foo"))
  }
}
