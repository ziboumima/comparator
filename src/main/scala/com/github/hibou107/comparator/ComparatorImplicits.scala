package com.github.hibou107.comparator

object ComparatorImplicits {

  def orderedComparator[A](implicit ordered: Ordering[A]): Comparator[A] = new Comparator[A] {
    override def compare(left: A, right: A)(implicit err: AcceptanceError): List[Diff] = {
      if (ordered.equiv(left, right))
        Nil
      else
        Diff(Nil, StringDiff(left.toString, right.toString)) :: Nil
    }
  }

  implicit val doubleComparator: Comparator[Double] = new Comparator[Double] {
    override def compare(left: Double, right: Double)(implicit err: AcceptanceError): List[Diff] = {
      val abs = Math.abs(left - right)
      val diff = Diff(Nil, DoubleDiff(left, right)) :: Nil
      if (left == 0.0 || right == 0.0)
        if (abs > err.absolute) diff else Nil
      else {
        val rel = abs / left
        if (rel > err.relative) diff else Nil
      }
    }
  }

  implicit val IntComparator: Comparator[Int] = orderedComparator[Int]
  implicit val LongComparator: Comparator[Long] = orderedComparator[Long]
  implicit val StringComparator: Comparator[String] = orderedComparator[String]

  implicit def seqComprator[A](implicit c: Comparator[A]): Comparator[Seq[A]] = new Comparator[Seq[A]] {
    def compare(left: Seq[A], right: Seq[A])(implicit err: AcceptanceError): List[Diff] = {
      if (left.lengthCompare(right.size) != 0)
        Diff(Nil, SizeDiff(left.size, right.size)) :: Nil
      else {
        left.zip(right).zipWithIndex.flatMap {
          case ((l, r), index) =>
            c.compareWithPath(index.toString, l, r)
        }.toList
      }
    }
  }

  implicit def mapComparator[K, V](implicit c: Comparator[V]): Comparator[Map[K, V]] = new Comparator[Map[K, V]] {
    def compare(left: Map[K, V], right: Map[K, V])(implicit err: AcceptanceError): List[Diff] = {
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
}