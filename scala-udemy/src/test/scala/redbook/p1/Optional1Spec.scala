package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class Optional1Spec extends AnyFunSpec {
  sealed trait Optional[+A] {
    def map[B](f: A => B): Optional[B]
    def flatmap[B](f: A => Optional[B]): Optional[B]
    def get: A
    def filter(fn: A => Boolean): Optional[A]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](default: => Optional[B]): Optional[B]
  }

  case object None extends Optional[Nothing] {
    override def map[B](f: Nothing => B): Optional[B] = None
    override def flatmap[B](f: Nothing => Optional[B]): Optional[B] = None
    override def get: Nothing = throw new Exception("empty object")
    override def filter(fn: Nothing => Boolean): Optional[Nothing] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](default: => Optional[B]): Optional[B] = default
  }

  case class Some[+A](private val v: A) extends Optional[A] {
    override def map[B](f: A => B): Optional[B] = Some(f(v))
    override def flatmap[B](f: A => Optional[B]): Optional[B] = f(v)
    override def get: A = v
    override def filter(fn: A => Boolean): Optional[A] =
      if(fn(v)) Some(v)
      else None
    override def getOrElse[B >: A](default: => B): B = v
    override def orElse[B >: A](default: => Optional[B]): Optional[B] = Some(v)
  }

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
