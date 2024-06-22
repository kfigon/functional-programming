package course.part2functional

import org.scalatest.funspec.AnyFunSpec

class MonadsSpec extends AnyFunSpec {
  // list, option, try, future, set - are monads
  // abstracting a computation or an effect
  trait Monad[A] {
    def apply(v: A): Monad[A] // aka unit
    def flatmap[B](f: A => Monad[B]): Monad[B] // aka bind
  }

  // laws:
  // left identity  apply(x).flatmap(f) == f(x)
  // right identity aMonad.flatmap(apply) == aMonad
  // associativity  aMonad.flatmap(f).flatmap(g) == aMonad.flatmap(x => f(x).flatmap(g))

  describe("my own monad") {

    // needs covariance
    sealed trait MyTry[+A] {
      def flatmap[B](f: A => MyTry[B]): MyTry[B]
    }

    object MyTry {
      // we need by name to avoid evaluation
      def apply[A](v: => A): MyTry[A] = try {
        Success(v)
      } catch {
        case t: Throwable => Fail(t)
      }
    }

    case class Success[+A](v: A) extends MyTry[A] {
      override def flatmap[B](f: A => MyTry[B]): MyTry[B] = try {
        f(v)
      } catch {
        case t: Throwable => Fail(t)
      }
    }

    case class Fail(e: Throwable) extends MyTry[Nothing] {
      override def flatmap[B](f: Nothing => MyTry[B]): MyTry[B] = this
    }
  }

  describe("my own monad2") {
    class Lazy[+A](v: => A) {
      private lazy val internal = v
      def get = internal
      def flatmap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internal)
    }

    object Lazy {
      def apply[A](v: => A): Lazy[A] = new Lazy[A](v)
    }

    it("apply") {
      var triggered = false
      val v = Lazy {
        triggered = true
        123
      }
      assert(!triggered)

      v.get
      assert(triggered)
    }

    it("flatmap") {
      var triggered = false
      val v = Lazy {
        triggered = true
        123
      }
      v.flatmap(v => Lazy(v + 3))
      assert(!triggered)

      v.get
      assert(triggered)
    }
  }
}
