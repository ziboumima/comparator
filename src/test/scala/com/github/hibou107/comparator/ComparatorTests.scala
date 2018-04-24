package com.github.hibou107.comparator

import com.github.hibou107.comparator.ComparatorImplicits._
import com.github.hibou107.comparator.GenericComparator._
import org.scalatest.{FlatSpec, Matchers}

class ComparatorTests extends FlatSpec with Matchers {

  implicit val acceptanceError: AcceptanceError = AcceptanceError(1e-2, 1e-5)

  it should "work with Tuple" in {
    Comparator.compare(((1, "x"), 2, 3), ((1, "y"), 2, 3)).head shouldBe
      Diff(List("_1", "_2"), StringDiff("x", "y"))
  }

  it should "work with List" in {
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.0, 3.0)) shouldBe List(Diff(List(), SizeDiff(2, 3)))
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.3)) shouldBe List(Diff(List("1"), DoubleDiff(2.0, 2.3)))
    Comparator.compare(List(1000.0, 2.0), List(999.0, 2.0)) shouldBe Nil // it must take into account acceptance error
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
    ) shouldBe List(
        Diff(List(), KeyNotExist("third", Left)),
        Diff(List(), KeyNotExist("second", Right))
      )

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 4.0)
    ) shouldBe List(Diff(List("second"), DoubleDiff(2.0, 4.0)))

  }

  it should "works in special case" in {
    sealed trait Tree
    case class Branch(left: Tree, right: Tree) extends Tree
    case class Leaf(value: Option[Double]) extends Tree

    val left = Branch(Leaf(Some(1.0)), Branch(Leaf(None), Leaf(Some(2.0))))
    val right = Branch(Leaf(Some(1.0)), Branch(Leaf(None), Leaf(Some(3.0))))
    Comparator.compare[Tree](left, right) shouldBe List(Diff(List("right", "right", "value"), DoubleDiff(2.0, 3.0)))

  }

}

