package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class Optional2Spec extends AnyFunSpec {
  sealed trait Optional[+A] {
    def map[B](f: A => B): Optional[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def flatmap[B](f: A => Optional[B]): Optional[B] = map(f).getOrElse(None)

    def get: A = this match {
      case None => throw new Exception("empty value")
      case Some(v) => v
    }

    def filter(fn: A => Boolean): Optional[A] = this match {
      case Some(v) if fn(v) => this
      case _ => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](default: => Optional[B]): Optional[B] = map(Some(_)).getOrElse(default)
  }

  case object None extends Optional[Nothing]
  case class Some[+A](private val v: A) extends Optional[A]

  val mapper = (v: Int) => v + 1
  val fmapper = (v: Int) => Some(v + 1)
  val even = (v: Int) => v % 2 == 0

  it("map") {
    assert(None.map(mapper) == None)
    assert(Some(123).map(mapper) == Some(124))
  }

  it("flatmap") {
    assert(None.flatmap(fmapper) == None)
    assert(Some(123).flatmap(fmapper) == Some(124))
    assert(Some(123).map(v => v).flatmap(fmapper) == Some(124))
  }

  it("filter") {
    assert(None.filter(even) == None)
    assert(Some(123).filter(even) == None)
    assert(Some(123).filter(v => !even(v)) == Some(123))
  }
  it("getOrElse") {
    assert(None.getOrElse(123) == 123)
    assert(Some(123).getOrElse(1) == 123)
  }

  it("orElse") {
    assert(None.orElse(Some(123)) == Some(123))
    assert(Some[Int](123).orElse(Some(1)) == Some(123))
  }
}
