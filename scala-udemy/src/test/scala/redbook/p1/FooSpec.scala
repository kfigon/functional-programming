package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class FooSpec extends AnyFunSpec {
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
}
