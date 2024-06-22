package course.part1syntax

import org.scalatest.funspec.AnyFunSpec

class SyntaxSugarSpec extends AnyFunSpec {
  it("methods with single param") {
    def foo(arg: Int): String = s"foo: $arg"

    val res = foo {
      // a lot of code
      123
    }
    assert(res == "foo: 123")

    //    it's used in Try{throw new RuntimeException("")
    //    or List(1,2,3).map { x => x + 1 }

  }

  it("single abstract method") {
    trait Action {
      def run(i: Int): Int
    }

    val foo = new Action {
      override def run(i: Int): Int = i + 1
    }

    //    lambda instead of full anonymous class when single method trait
    //    used in Runnables for Threads
    val eqFoo: Action = (x: Int) => x + 1

    assert(foo.run(4) == eqFoo.run(4))
  }

  describe("special methods :: #::") {
    it("prepend list") {
      //    :: - prepend list, infix method
      //    #:: prepends on streams
      //    associativity of method. If it ends with `:`, it's right associative. Right evaluated first
      val l = 2 :: List(3, 4) // eq List(3,4).::(2)
      assert(l == List(2, 3, 4))

      val x = 1 :: 2 :: 3 :: 4 :: List()
      val y: List[Int] = List().::(4).::(3).::(2).::(1)
      assert(x == List(1, 2, 3, 4))
      assert(x == y)
    }
  }

  it("multiword method") {
    class Foo(name: String) {
      def `and then said`(d: String): String = s"$name said $d"
    }

    val f = new Foo("asd")
    val res = f `and then said` "scala magick"

    assert(res == "asd said scala magick")
  }

  it("infix type") {
    case class Composite[A, B](a: A, b: B)
    val comp: Composite[Int, String] = Composite(1, "foo")
    val comp2: Int Composite String = Composite(1, "foo")

//    this allows such constructs:
//    class -->[A,B]
//    val foo: Int --> String = ???


    assert(comp == comp2)
  }

  describe("mutable containers") {
    it("update method") {
      // apply is also special

      val arr = Array(1, 2, 3) // mutable list
      arr(2) = 7 // rewritten as arr.update(2,7)
      assert(arr sameElements Array(1, 2, 7))
    }

    it("setter methods") {
      class Mut {
        private var internalMember: Int = 0

        //getter and setter
        def memb: Int = internalMember
        def memb_=(v: Int):Unit = internalMember = v
      }

      val v = new Mut
      assert(v.memb == 0)
      v.memb = 42
      assert(v.memb == 42)
    }
  }
}
