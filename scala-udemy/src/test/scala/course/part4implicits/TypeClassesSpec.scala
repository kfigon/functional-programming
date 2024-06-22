package course.part4implicits

import org.scalatest.funspec.AnyFunSpec

import java.util.Date
import scala.language.implicitConversions

//traits that use implicits (like Ordering[_]) are typeclasses (like interfaces from haskell)
//defined implicit orderings are typeclass instances

//typeclasses are not supertypes of some class, these are pure interfaces. You don't accept typeclass instance as params
// you retain full type, no type erasure/subtyping

// normal traits work for type we own, we need to implement wrappers if we need to support other types

// the power over regular traits:
// we still have full type, not a subtype (aka just the trait object)
// we can implement it for foreign types
// we can have multiple implementations for single type - see Equality for User and UserName here
class TypeClassesSpec extends AnyFunSpec {

  describe("just trait") {
    // we need to own type
    // we can have only one toHtml impl
    trait HTMLWritable {
      def toHtml: String
    }

    case class User(name: String, age: Int) extends HTMLWritable {
      override def toHtml: String = s"<div>$name, $age</div>"
    }

    it("my type works") {
      val u = User("John", 30)
      assert("<div>John, 30</div>" == u.toHtml)
    }
  }

  describe("html typeclass") {
    // typeclass - trait with type parameter and methods
    // we can implement it for types we don't own
    trait HTMLSerializer[T] {
      def serialize(v: T): String
    }

    case class User(name: String, age: Int)

    // typeclass instances, singleton objects
    object UserSerializer extends HTMLSerializer[User] {
      override def serialize(v: User): String = s"<div>${v.name}, ${v.age}</div>"
    }

    object JavaDateSerializer extends HTMLSerializer[java.util.Date] {
      override def serialize(v: Date): String = s"<div>${v.toString}</div>"
    }

    object IntSerializer extends HTMLSerializer[Int] {
      override def serialize(v: Int): String = s"<div>$v</div>"
    }

    it("user typeclass") {
      val u = User("John", 30)
      assert("<div>John, 30</div>" == UserSerializer.serialize(u))
    }

    it("int typeclass") {
      assert("<div>123</div>" == IntSerializer.serialize(123))
    }
  }

  describe("equality typeclass") {
    trait Equal[T] {
      def apply(a: T, b: T): Boolean
    }
    case class User(name: String, age: Int)

    //  this is the power over regular traits - we can have multiple implementations
    object UserNameEq extends Equal[User] {
      override def apply(a: User, b: User): Boolean = a.name == b.name
    }

    object UserEq extends Equal[User] {
      override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age
    }

    it("user eq") {
      val u1 = User("John", 30)
      val u2 = User("John", 32)

      assert(UserNameEq(u1,u2))
      assert(!UserEq(u1,u2))
    }
  }

  describe("typeclass with implicit") {
    trait Printer[T] {
      def show(v: T): String
    }

    object Printer {
      def print[T](v: T)(implicit printer: Printer[T]): String = printer.show(v)


      // this is nice, as we can then use Printer[User].show(v)
      // def apply[T](implicit serializer: Printer[T]): Printer[T] = serializer
    }

    case class User(name: String, age: Int)

    object Serializers {
      implicit object FullUserSerializer extends Printer[User] {
        override def show(v: User): String = s"${v.name}, ${v.age}yo"
      }

      implicit object UserNameSerializer extends Printer[User] {
        override def show(v: User): String = s"${v.name}"
      }

      implicit object IntSerializer extends Printer[Int] {
        override def show(v: Int): String = s"data: $v"
      }
    }

    it("full user") {
      import Serializers.FullUserSerializer
      val u = User("John", 30)
      assert(Printer.print(u) == "John, 30yo")
    }

    it("simple user") {
      import Serializers.UserNameSerializer
      val u = User("John", 30)
      assert(Printer.print(u) == "John")
    }

    it("int") {
      import Serializers.IntSerializer
      val u = 123
      assert(Printer.print(u) == "data: 123")
    }
  }

  describe("equality typeclass with implicit") {
    trait Eq[T] {
      def apply(a: T, b: T): Boolean
    }

    object Eq {
      def apply[T](a: T, b: T)(implicit equ: Eq[T]): Boolean = equ(a,b)
    }

    it("int eq") {
      implicit def intEq: Eq[Int] = (a,b) => a == b

      assert(!Eq(3, 2))
      assert(Eq(3, 3))
    }

    it("person eq") {
      case class Person(name: String, age: Int)
      implicit def personEq: Eq[Person] = (a,b) => a.name == b.name && a.age == b.age

      assert(!Eq(Person("foo", 3), Person("foo", 4)))
      assert(Eq(Person("foo", 3), Person("foo", 3)))
    }
  }

  describe("overengineered advent of code template") {
    val inputData =
      """1
        |12
        |42
        |67
        |33
        |18
        |17""".stripMargin

    case class Data(vals: List[Int])

    implicit def parse(s: String): Data = Data(s.linesIterator.map(_.toInt).toList)

    trait Solver[T] {
      def solve(v: T): Int
    }

    def solve(v: Data)(implicit solver: Solver[Data]): Int = solver.solve(v)

    object Solutions {
      implicit object P1Solver extends Solver[Data] {
        override def solve(v: Data): Int = v.vals.sum
      }

      implicit object P2Solver extends Solver[Data] {
        override def solve(v: Data): Int = v.vals.count(_ >= 18)
      }
    }

    it("p1 - implicit conversion") {
      import Solutions.P1Solver
      // implicit conversion String -> Data (parse = String => Data)
      // calling solve with implicit resolving typeclass Solver for part1
      assert(solve(inputData) == 190)
    }

    it("p2 - implicit conversion") {
      import Solutions.P2Solver
      assert(solve(inputData) == 4)
    }

    it("enrich string with typeclass") {
      // convert and solve
      implicit object P1StrSolver extends Solver[String] {
        override def solve(v: String): Int = parse(v).vals.sum
      }

      // adds solveTask to string
      implicit class SolveEnricher[T](v: T) {
        def solveTask(implicit solver: Solver[T]) = solver.solve(v)
      }

      assert(inputData.solveTask == 190)
    }
  }

  describe("pimp by lib exercise") {
    // write Eq[T] typeclass
    // add new operator to 2 classes and enrich them
    // need to:
    // - define typeclass
    // - add typeclass instances for both classes
    // - add enricher, that uses typeclass instances to

    case class Foo(v: Int)

    trait Eq[T] {
      def apply(a: T, b: T): Boolean
    }

    implicit object StrEq extends Eq[String] {
      override def apply(a: String, b: String): Boolean = a == b
    }

    implicit object FooEq extends Eq[Foo] {
      override def apply(a: Foo, b: Foo): Boolean = a.v == b.v
    }

    implicit class EqualityEnricher[T](v: T) {
      def ===&(other: T)(implicit eq: Eq[T]): Boolean = eq(v, other)
    }


    it("string") {
//      new EqualityEnricher("foo").===&(new EqualityEnricher("foo"))(Eq.StrEq)
      assert("foo" ===& "foo")
    }

    it("Foo") {
      assert(Foo(1) ===& Foo(1))
    }
  }
}
