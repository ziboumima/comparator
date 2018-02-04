package com.github.hibou107.comparator

import org.scalatest.{ FlatSpec, Matchers }
import ComparatorImplicits._
import GenericComparator._
class ComparatorTests extends FlatSpec with Matchers {

  implicit val acceptanceError: AcceptanceError = AcceptanceError(1e-2, 1e-5)

  it should "work with Tuple" in {
    println(Comparator[(Double, Double, Double)].compare((1, 2, 3), (2, 3, 4)))
  }

  it should "work with List" in {
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.0, 3.0)) shouldBe List(Diff(List(), SizeDiff(2, 3)))
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.3)) shouldBe List(Diff(List("1"), DoubleDiff(2.0, 2.3)))
    Comparator.compare(List(1000.0, 2.0), List(999.0, 2.0)) shouldBe Nil // it must take into account acceptence error
  }

  it should "compare case class" in {
    case class Toto(x: Int, y: String)
    Comparator[Toto].compare(Toto(1, "x"), Toto(1, "x")) shouldBe Nil
    Comparator[Toto].compare(Toto(1, "first"), Toto(1, "second")) shouldBe List(Diff(List("y"), StringDiff("first", "second")))
  }

  it should "work with Map" in {
    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 2.0)
    ) shouldBe Nil

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "third" -> 2.0)
    ) shouldBe List(Diff(List(), KeyNotExist("third", Left)))

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 4.0)
    ) shouldBe List(Diff(List("second"), DoubleDiff(2.0, 4.0)))

  }

  it should "works in special case" in {
    sealed trait Tree
    case class Branch(left: Tree, right: Tree) extends Tree
    case object Empty extends Tree
    case class Leaf(value: Double) extends Tree

    implicit def treeComparator(implicit doubleComparator: Comparator[Double]): Comparator[Tree] = new Comparator[Tree] {
      def compare(left: Tree, right: Tree)(implicit err: AcceptanceError): List[Diff] =
        (left, right) match {
          case (Empty, Empty)                   => Nil
          case (Leaf(l), Leaf(r))               => doubleComparator.compare(l, r)(err)
          case (Branch(ll, lr), Branch(rl, rr)) => compareWithPath("left", ll, rl)(err) ++ compareWithPath("right", lr, rr)(err)
          case _                                => Diff(Nil, TypeDiff) :: Nil

        }
    }

    val left = Branch(Leaf(1.0), Branch(Empty, Leaf(2.0)))
    val right = Branch(Leaf(1.0), Branch(Empty, Leaf(3.0)))

    Comparator.compare(left, right) shouldBe List(Diff(List("right", "right"), DoubleDiff(2.0, 3.0)))

  }

}
