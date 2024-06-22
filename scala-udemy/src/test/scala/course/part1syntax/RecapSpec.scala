package course.part1syntax

import org.scalatest.funspec.AnyFunSpec

class RecapSpec extends AnyFunSpec {

  trait ATrait {
    def foo(a: Int): Int
  }

  // or `with` if there are multiple
  class Foo extends ATrait{
    override def foo(a: Int): Int = a + 1
  }

  // generics. + singifies covariance
  class MyList[+A]

  describe("traits, generics") {
    it("method notation") {
      val f = new Foo()
      assert(f.foo(2)== 3)
      assert((f foo 2) == 3)

      val anonymousClass = new ATrait {
        override def foo(a: Int): Int = a*2
      }
      assert(anonymousClass.foo(2) == 4)
    }
  }

  it("exceptions") {
    val res = try {
      throw new Exception("oops")
    } catch {
      case e: Exception => "got"
    } finally {
      // cleanup...
    }

    assert(res == "got")
  }

  describe("FP") {
    it("functions") {
      //    this is how scala implements functions in JVM - anonymous fun. Apply method is special
      val inc = new Function1[Int, Int] {
        override def apply(v1: Int): Int = v1 + 1
      }

      val inc2 = (v: Int) => v + 1

      assert(inc(1) == inc2(1))
    }

    it("for comprehension") {
      // cross pair, chain of flatmaps
      val nums = List(1, 2,3,4)
      val chars = List('a', 'b', 'c')
      val pairs: List[(Int, Char)] = for {
        n <- nums
        c <- chars
      } yield (n, c)

      val pairs2: List[(Int, Char)] = nums.flatMap(n => {
        chars.map(c => (n, c))
      })

      assert(pairs == pairs2)
      assert(pairs == List(
        (1,'a'), (1,'b'), (1,'c'),
        (2,'a'), (2,'b'), (2,'c'),
        (3,'a'), (3,'b'), (3,'c'),
        (4,'a'), (4,'b'), (4,'c'),
      ))
    }
  }

  it("call by name - lazy eval, like lambda - but not quite") {
    def foo(v: => Int) = v + 1

    assert(foo(4) == 5)
  }
}
