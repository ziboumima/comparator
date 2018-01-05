package com.github.hibou107.Comparator

object ComparatorImplicits {

  def comparator2[A1, A2](names: (String, String))(implicit comparator1: Comparator[A1],
                                                   comparator2: Comparator[A2],
                                                   err: AcceptanceError): Comparator[(A1, A2)] =
    new Comparator[(A1, A2)] {
      def compare(left: (A1, A2), right: (A1, A2))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2)
    }


  implicit val doubleComparator: Comparator[Double] {
  } = new Comparator[Double] {
    def compare(left: Double, right: Double)(implicit err: AcceptanceError): List[Diff] = {
      val abs = Math.abs(left - right)
      val diff = Diff(Nil, DoubleDiff(left, right)) :: Nil
      if (left == 0.0 || right == 0.0)
        if (abs > err.absolute) diff else Nil
      else
        {
          val rel = abs / left
          if (rel > err.relative) diff else Nil
        }
    }

  }

  implicit def seqComprator[A](implicit c: Comparator[A]): Comparator[Seq[A]] = new Comparator[Seq[A]] {
    def compare(left: Seq[A], right: Seq[A])(implicit err: AcceptanceError): List[Diff] = {
      if (left.size != right.size)
        Diff(Nil, SizeDiff(left.size, right.size)) :: Nil
      else
      {
        left.zip(right).zipWithIndex.flatMap { case ((l, r), index) =>
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
        left.foldLeft(List.empty[Diff]) { case (currentResult, (leftKey, leftVal)) =>
          val temp = right.get(leftKey).fold(Diff(Nil, KeyNotExist(leftKey.toString, Right)) :: Nil) { rightVal =>
            c.compareWithPath(leftKey.toString, leftVal, rightVal)
          }
          temp ++ currentResult
        }
    }
  }
}