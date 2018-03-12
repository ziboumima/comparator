package com.github.hibou107.comparator

import shapeless.{ :+:, CNil, Coproduct, HList, HNil, TypeClass, TypeClassCompanion }

object ComparatorImplicits {

  def orderedComparator[A](implicit ordered: Ordering[A]): Comparator[A] = (left: A, right: A) => {
    if (ordered.equiv(left, right))
      Nil
    else
      Diff(Nil, StringDiff(left.toString, right.toString)) :: Nil
  }

  implicit def doubleComparator(implicit err: AcceptanceError): Comparator[Double] = (left: Double, right: Double) => {
    val abs = Math.abs(left - right)
    val diff = Diff(Nil, DoubleDiff(left, right)) :: Nil
    if (left == 0.0 || right == 0.0)
      if (abs > err.absolute) diff else Nil
    else {
      val rel = abs / left
      if (rel > err.relative) diff else Nil
    }
  }

  implicit val IntComparator: Comparator[Int] = orderedComparator[Int]
  implicit val LongComparator: Comparator[Long] = orderedComparator[Long]
  implicit val StringComparator: Comparator[String] = orderedComparator[String]

  implicit def listComparator[A](implicit c: Comparator[A]): Comparator[List[A]] = (left: List[A], right: List[A]) => {
    if (left.lengthCompare(right.size) != 0)
      Diff(Nil, SizeDiff(left.size, right.size)) :: Nil
    else {
      left.zip(right).zipWithIndex.flatMap {
        case ((l, r), index) =>
          c.compareWithPath(index.toString, l, r)
      }
    }
  }

  implicit def optionComparator[A](implicit c: Comparator[A]): Comparator[Option[A]] = (left: Option[A], right: Option[A]) => {
    (left, right) match {
      case (Some(l), Some(r)) => c.compare(l, r)
      case (None, None)       => Nil
      case (x, y)             => Diff(Nil, TypeDiff(x.toString, y.toString)) :: Nil
    }
  }

  implicit def mapComparator[K, V](implicit c: Comparator[V]): Comparator[Map[K, V]] = (left: Map[K, V], right: Map[K, V]) => {
    val keydiff = right.keySet -- left.keySet
    if (keydiff.nonEmpty)
      keydiff.map(diff => Diff(Nil, KeyNotExist(diff.toString, Left))).toList
    else
      left.foldLeft(List.empty[Diff]) {
        case (currentResult, (leftKey, leftVal)) =>
          val temp = right.get(leftKey).fold(Diff(Nil, KeyNotExist(leftKey.toString, Right)) :: Nil) { rightVal =>
            c.compareWithPath(leftKey.toString, leftVal, rightVal)
          }
          temp ++ currentResult
      }
  }
}
