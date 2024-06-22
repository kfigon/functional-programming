package course.part1syntax

import org.scalatest.funspec.AnyFunSpec

class AdvancedPatternMatching extends AnyFunSpec {
  it("head tail list") {
    val h = List(1) match {
      case Nil => 42
      case ::(head, tl) => head
    }
    assert(h == 1)
  }

  describe("unapply") {
    it("basic") {
      // normally case classes can be destructured, but classes not
      // used when for some reason if we can't make it case class.
      // we implement unapply companion method to deconstruct it
      class Person(val name: String, val age: Int)

      object Person { // this can be renamed whatever, and then used in pattern. case PersonPattern(_,_) => ??? can also work
        def unapply(p: Person): Option[(String, Int)] = Some((p.name, p.age))
      }

      val b = new Person("bob", 42)

      val age = b match {
        case Person(_, age) => age
      }
      assert(age == 42)
    }

    it("more unapplies") {
      class Person(val name: String, val age: Int)

      object Person {
        def unapply(p: Person): Option[(String, Int)] = Some((p.name, p.age)) // we can also return None for some interesting patterns

        def unapply(age: Int): Option[String] = Some( // unapply on Person, so it matches
          if (age >= 18) "adult"
          else "minor"
        )
      }

      val betty = new Person("Betty", 17) // this is not required btw, 20 match {} will also work

      val status = betty.age match {
        case Person(status) => status
      }

      assert(status == "minor")
    }

    it("own int matcher") {
      object IntMatcher {
        def unapply(v: Int): Option[String] = Some(
          if (v < 10) "single digit"
          else if (v % 2 == 0) "even"
          else "no property"
        )
      }

      val results = List(1, 10, 11).map {
        case IntMatcher(v) => v
      }
      assert(results == List("single digit", "even", "no property"))
    }

    it("more matchers") {
      object SingleDigitMatcher {
        def unapply(v: Int): Option[String] = if (v < 10) Some("single digit") else None
      }

      object Even {
        def unapply(v: Int): Option[String] = if (v % 2 == 0) Some("even") else None
      }

      object Other {
        def unapply(v: Int): Boolean = true // we can return booleans without options
      }

      val results = List(1, 10, 11).map {
        case SingleDigitMatcher(v) => v
        case Even(v) => v
        case Other() => "no property"
      }
      assert(results == List("single digit", "even", "no property"))
    }
  }

  it("infix type patterns") {
    case class Or[A, B](a: A, b: B)
    val or = Or(2, "two")

    val v = or match {
      // equivalent: case Or(num, str) => s"$num -> $str"
      case num Or str => s"$num -> $str"
    }
    assert(v == "2 -> two")
  }

  it("decompose sequences") {
    val l = List(1,2,3)

    val foo = l match {
      case List(1, _*) => "starts with 1"
      case _ => "other"
    }

    assert(foo == "starts with 1")
  }

  it("unapplySeq") {
//    when we want to use unapplySeq on our own types
    abstract class MyList[+A] {
      def head: A = ???
      def tail: MyList[A] = ???
    }

    case object Empty extends MyList[Nothing]
    case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

    object MyList {
      def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
        if (list == Empty) Some(Seq.empty)
        else unapplySeq(list.tail).map(list.head +: _) // prepent option head to tail
    }

    val myList = Cons(1, Cons(2, Cons(3, Empty)))
    val decomposed = myList match {
      case MyList(1, 2, _*) => true
      case _ => false
    }

    assert(decomposed)
  }

  it("custom returns types for unapply") {
    // unapply return type - it doesnt have to be bool or option
    // we need 2 methods - isEmpty: Boolean and get: something
    abstract class Wrapper[T] {
      def isEmpty: Boolean
      def get: T
    }
    class Person(val name: String, val age: Int)

    object PersonWrapperPat {
      def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
        override def isEmpty: Boolean = false
        override def get: String = person.name
      }
    }

    val p = new Person("bob", 42)

    val name = p match {
      case PersonWrapperPat(n) => n
    }
    assert(name == "bob")
  }
}
