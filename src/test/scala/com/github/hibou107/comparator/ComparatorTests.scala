package com.github.hibou107.comparator

import org.scalatest.{FlatSpec, Matchers}
import ComparatorImplicits._

class ComparatorTests extends FlatSpec with Matchers {

  implicit val acceptanceError: AcceptanceError = AcceptanceError(1e-2, 1e-5)

  it should "work with Tuple2" in {
    val c = comparator2[Double, Double](("first", "second"))

    c.compare((1, 2), (3, 4)) shouldBe List(
      Diff(List("first"),DoubleDiff(1.0, 3.0)),
      Diff(List("second"),DoubleDiff(2.0, 4.0)))

    c.compare((1, 2), (1, 2)) shouldBe Nil

    c.compare((1, 4), (1, 2)) shouldBe List(
      Diff(List("second"), DoubleDiff(4, 2))
    )
  }

  it should "work with Tuple2[Tuple2]" in {
    implicit def c2[A, B](implicit a: Comparator[A], b: Comparator[B]): Comparator[(A, B)] =
      comparator2[A, B](("first", "second"))

    val result = Comparator.compare(
      ((1.0, 2.0), (2.0, 2.0)),
      ((1.0, 2.0), (1.0, 2.0)))

    result shouldBe List(
      Diff(List("second", "first"), DoubleDiff(2.0, 1.0)))
  }


  it should "work with List" in {
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.0, 3.0)) shouldBe List(Diff(List(),SizeDiff(2,3)))
    Comparator.compare(List(1.0, 2.0), List(1.0, 2.3)) shouldBe List(Diff(List("1"), DoubleDiff(2.0,2.3)))
    Comparator.compare(List(1000.0, 2.0), List(999.0, 2.0)) shouldBe Nil // it must take into account acceptence error
  }

  it should "work with Map" in {
    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 2.0)
    ) shouldBe Nil

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "third" -> 2.0)
    ) shouldBe List(Diff(List(),KeyNotExist("third",Left)))

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 4.0)
    ) shouldBe List(Diff(List("second"), DoubleDiff(2.0, 4.0)))

  }


}
