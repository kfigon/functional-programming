package course.part5typesystem

import org.scalatest.funspec.AnyFunSpec

class TypeSystemSpec extends AnyFunSpec {
  trait Animal
  case class Dog() extends Animal
  case class Cat() extends Animal
  case class Crocodile() extends Animal

  // variance - type substition of generics. Useful concept when modeling containers

  // should a Cage[Cat] inherit from Animal?
  // yes => Covariance. Then we do class Cage[+T]
  // no => Invariance. Types are completely different
  // very no => ContraVariance. Types are completely different, oposite
  class Cage[T]
  class CovCage[+T]
  class ContraCage[-T]

  it("variance") {

    val c: CovCage[Animal] = new CovCage[Cat] // without +, it does not compile
    // language=scala
    assertDoesNotCompile("val x: Cage[Animal] = new Cage[Cat]")
    val x: ContraCage[Cat] = new ContraCage[Animal]
  }

  it("type members - we can specify what goes in") {

    class AnimalCollection {
      type AnimalType // abstract type
      type Bounded <: Animal
      type SuperBounded >: Dog <: Animal

      type AnimalC = Cat
    }
    // we can specify types

    val ac = new AnimalCollection
    val cat: ac.AnimalC = new Cat
    val d: ac.SuperBounded = new Dog
    // language=scala
    assertDoesNotCompile("val e: ac.Bounded = new Dog")
  }

  it("self type") {
    // requiring a type to be mixed in

    trait CanPlayInstrument {
      def plan(): Unit
    }

    // Singer requires CanPlayInstrument
    trait Singer { self: CanPlayInstrument => // whoever implements this trait, should also implement CanPlayInstrument
      def sing(): Unit
    }

    // this is different - this is Singer IS CanPlayInstrument
    // trait Singer with CanPlayInstrument


    // language=scala
    assertDoesNotCompile("""
    class Foo extends Singer {
      override def sing(): Unit = ()
    }
    """)
  }

  it("self type - cake pattern for DI") {

    // classical DI
    trait Dependency {
      def action: String
    }
    class InstanceA extends Dependency {
      override def action: String = "A"
    }
    class Component(val dep: Dependency) {
      def foo: String = s"foo: ${dep.action}"
    }

    // cake pattern
    trait ScalaDependency {
      def action: String
    }
    trait ScalaComponent {
      self: ScalaDependency =>
      // Scala componenet also "has" action method
      def foo: String = s"foo: $action"
    }
    // has to be traits to be mixed in
    trait Test extends ScalaDependency {
      override def action: String = "test"
    }
    trait Prod extends ScalaDependency {
      override def action: String = "prod"
    }

    // cake pattern. DI in compile time
    class MyTestingApp extends ScalaComponent with Test

    val m = new MyTestingApp
    assert(m.foo == "foo: test")
  }

  it("recursive types - F-bounded polymorphism") {
    trait Animalz {
      def breed: List[Animalz]
    }

    // compiler does not require me to be correct with type here
    class Catz extends Animalz {
      override def breed: List[Catz] = ???
    }

    // better - recursive type: F-bounded polymorhism
    trait AnimalzBetter[A <: AnimalzBetter[A]] {
      def breed: List[AnimalzBetter[A]]
    }

    // compiler enforces the type
    class CatBetter extends AnimalzBetter[CatBetter]{
      override def breed: List[AnimalzBetter[CatBetter]] = ???
    }

    // solution 3 - F bound polymorhism + self type
    // compiler enforces the type the best

    // or just use typeclasses...
    trait AnimalzBest[A <: AnimalzBest[A]] { self: A => }
  }

  it("Higher kinded types") {
    // deep generic types with unknown type in the deepest level

    object Naive {
      // what if we want flatmap trait?
      trait MyList[T] {
        def flatmap[B](f: T => MyList[B]): MyList[B]
      }

      trait MyOption[T] {
        def flatmap[B](f: T => MyOption[B]): MyOption[B]
      }
    }

    trait FlatMappable[F[_], A] {
      def flatmap[B](f: A => F[B]): F[B]
    }

  }
}
