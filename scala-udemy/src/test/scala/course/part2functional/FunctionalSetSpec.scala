package course.part2functional

import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable

class FunctionalSetSpec extends AnyFunSpec {
  describe("my own set") {

    // our set is just a function. Maps and Lists are partial functions (an elem might not be present in those)
    trait MySet[A] extends (A => Boolean) {
//      def apply(seq: Seq[A]): MySet[A] // extends (A => Boolean) will require this, no need to define it here

      def contains(a: A): Boolean
      def +(a: A): MySet[A]
      def ++(other: MySet[A]): MySet[A] // concat set, aka union sets

      def map[B](f: A => B): MySet[B]
      def flatmap[B](f: A => MySet[B]): MySet[B]
      def filter(f: A => Boolean): MySet[A]
      def foreach(f: A => Unit): Unit
    }

    // we could use a linked list with sum types to be more functional. This is a bit imperative
    class MySetImpl[A](private val data: Seq[A]) extends MySet[A] {

      override def apply(a: A): Boolean = contains(a)

      override def contains(a: A): Boolean = data.contains(a)

      override def +(a: A): MySet[A] = {
        val newSeq: Seq[A] = data.+:(a)
        MySetImpl[A](newSeq:_*)
      }

      override def ++(other: MySet[A]): MySet[A] = {
        var out = MySetImpl[A](data:_*)
        for(i <- other) {
          out = out + i
        }
        out
      }

      override def map[B](f: A => B): MySet[B] = MySetImpl(data.map(f):_*)

      override def flatmap[B](f: A => MySet[B]): MySet[B] = {
        var out = MySetImpl[B]()
        for(i <- data) {
          out = out ++ f(i)
        }
        out
      }

      override def filter(f: A => Boolean): MySet[A] = MySetImpl[A](data.filter(f):_*)

      override def foreach(f: A => Unit): Unit = data.foreach(f)
    }

    object MySetImpl {
      // vararg
      // to use iter on this: :_*
      def apply[A](seq: A*): MySet[A] = new MySetImpl(seq.toSet.toSeq)
    }

    it("myown set"){
      val s = MySetImpl(1,2,3)
      assert(s(1))
      assert(!s(4))
    }

    def collect[A](m: MySet[A]): List[A] = {
      val out = mutable.ArrayBuffer[A]()
      m.foreach(out.append(_))

      out.toList
    }

    it("filter") {
      val s = MySetImpl(1,2,3,4)
      val res = s.filter(_ % 2 == 0)
      val r = collect(res).sorted
      assert(r == List(2,4))
    }

    it("map") {
      val s = MySetImpl(1,2,3,4)
      val res = s.map(_.toString)
      val r = collect(res).sorted
      assert(r == List("1", "2", "3", "4"))
    }

    it("flatmap") {
      val s = MySetImpl(1,2,3,4)
      val res = s.flatmap(v => MySetImpl(v+1))
      val r = collect(res).sorted
      assert(r == List(2, 3, 4, 5))
    }
  }
}
