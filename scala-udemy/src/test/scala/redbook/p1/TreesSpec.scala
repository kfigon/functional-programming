package redbook.p1

import org.scalatest.funspec.AnyFunSpec

class TreesSpec extends AnyFunSpec {

  sealed trait Tree[+A]
  case class Leaf[+A](v: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left)+size(right) + 1
    }

    // this is hard
    def sizeTco[A](t: Tree[A]): Int = {
      @scala.annotation.tailrec
      def run(t: List[Tree[A]], acc: Int): Int = t match {
        case Nil => acc
        case Leaf(_) :: tail => run(tail, 1+acc)
        case Branch(l,r) :: tail => run(l::r::tail, acc + 1)
      }
      run(List(t), 0)
    }

    def max(t: Tree[Int]): Int = {
      val maxVal = (m: Option[Int], v: Int) => m.map(_.max(v)).getOrElse(v)

      def run(t: Tree[Int], maximum: Option[Int]): Int = t match {
        case Leaf(v) => maxVal(maximum, v)
        case Branch(left, right) =>
          val leftMax = run(left, maximum)
          val rightMax = run(right, maximum)
          leftMax.max(rightMax)
      }

      run(t, None)
    }

    def depth[A](t: Tree[A]): Int = {
      def run(t: Tree[A], d: Int): Int = t match {
        case Leaf(_) => d
        case Branch(left, right) =>
          val leftDept = run(left, d+1)
          val rightDept = run(right, d+1)
          leftDept.max(rightDept)
      }

      run(t, 0)
    }

    def depth2[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth2(left).max(depth2(right))
    }

    def map[A,B](t: Tree[A])(fn: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(fn(v))
      case Branch(left, right) => Branch(map(left)(fn), map(right)(fn))
    }

    def fold[A,B](t: Tree[A])(fn: A => B)(combine: (B,B) => B): B = t match {
      case Leaf(v) => fn(v)
      case Branch(left, right) =>
        val lReduced = fold(left)(fn)(combine)
        val rReduced = fold(right)(fn)(combine)
        combine(lReduced, rReduced)
    }
  }

  val exampleTree = Branch(
    Branch(
      Leaf(1),
      Branch(
        Branch(
          Leaf(2),
          Leaf(3)),
        Leaf(4))),
    Leaf(5)
  )

  val exampleTree2 = Branch(
    Branch(
      Leaf(1),
      Branch(
        Branch(
          Branch(
              Leaf(80),
              Leaf(-1)),
          Leaf(3)),
        Leaf(4))),
    Leaf(5)
  )

  val doubledExample = Branch(
    Branch(
      Leaf(2),
      Branch(
        Branch(
          Leaf(4),
          Leaf(6)),
        Leaf(8))),
    Leaf(10)
  )

  it("ex25 size") {
    assert(Tree.size(exampleTree) == 9)
    assert(Tree.sizeTco(exampleTree) == 9)
  }

  it("ex26 max") {
    assert(Tree.max(exampleTree) == 5)
    assert(Tree.max(exampleTree2) == 80)
  }

  it("ex27 depth") {
    assert(Tree.depth(exampleTree) == 4)
    assert(Tree.depth2(exampleTree) == 4)

    assert(Tree.depth(exampleTree2) == 5)
    assert(Tree.depth2(exampleTree2) == 5)
  }

  it("ex28 map") {
    assert(Tree.map(exampleTree)(_ * 2) == doubledExample)
  }

  describe("ex29 fold") {
    it("size"){
      assert(Tree.fold(exampleTree)((_) =>  1)((left,right) => left + right + 1) == 9)
    }

    it("max"){
      val f = (v: Int) => v
      val g = (a: Int, b: Int) => a max b

      assert(Tree.fold(exampleTree)(f)(g) == 5)
      assert(Tree.fold(exampleTree2)(f)(g) == 80)
    }
    it("depth"){
      val f = (a: Int) => 1
      val g = (a: Int,b: Int) => 1 + a max b

      assert(Tree.fold(exampleTree)(f)(g) == 4)
      assert(Tree.fold(exampleTree2)(f)(g) == 5)
    }
    it("map"){
      val f: Int => Tree[Int] = (a: Int) => Leaf(a*2)
      val g = (a: Tree[Int],b: Tree[Int]) => Branch(a,b)

      assert(Tree.fold(exampleTree)(f)(g) == doubledExample)
    }
  }
}
