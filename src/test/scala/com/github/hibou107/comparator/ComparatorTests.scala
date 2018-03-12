package com.github.hibou107.comparator

import org.scalatest.{ FlatSpec, Matchers }
import ComparatorImplicits._
import GenericComparator._
import shapeless._

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
    ) shouldBe List(Diff(List(), KeyNotExist("third", Left)))

    Comparator.compare(
      Map("first" -> 1.0, "second" -> 2.0),
      Map("first" -> 1.0, "second" -> 4.0)
    ) shouldBe List(Diff(List("second"), DoubleDiff(2.0, 4.0)))

  }

  it should "works in special case" in {
    sealed trait Tree
    case class Branch(left: Tree, right: Tree) extends Tree
    case class Leaf(value: Option[Double]) extends Tree

//    implicit def treeComparator: Comparator[Tree] = new Comparator[Tree] {
//      def compare(left: Tree, right: Tree)(implicit err: AcceptanceError): List[Diff] =
//        (left, right) match {
//          case (Leaf(l), Leaf(r))               => Comparator.compare(l, r)(implicitly, err)
//          case (Branch(ll, lr), Branch(rl, rr)) => compareWithPath("left", ll, rl)(err) ++ compareWithPath("right", lr, rr)(err)
//          case (x, y)                           => Diff(Nil, TypeDiff(x.toString, y.toString)) :: Nil
//
//        }
//    }
    import scala.reflect.runtime.universe._

    val left = Branch(Leaf(Some(1.0)), Branch(Leaf(None), Leaf(Some(2.0))))
    val right = Branch(Leaf(Some(1.0)), Branch(Leaf(None), Leaf(Some(3.0))))
    println(reify(Comparator[Tree]))
    Comparator.compare[Tree](left, right) shouldBe List(Diff(List("right", "right"), DoubleDiff(2.0, 3.0)))

  }

  it should "works with coProduct" in {
    import scala.reflect.runtime.universe._
    sealed trait People {
      val name: String
    }
    case class Man(name: String) extends People
    case class Woman(name: String) extends People
    val man = Man("Man")
    val woman = Woman("Woman")
    println(LabelledGeneric[People].to(man))
    println(LabelledGeneric[People].to(woman))
//    println(Comparator[People])
//    println(reify(Comparator[People]))

  }


}

