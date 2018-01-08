package com.github.hibou107.comparator

object ComparatorImplicits {


  implicit val doubleComparator: Comparator[Double] {
  } = new Comparator[Double] {
    def compare(left: Double, right: Double)(implicit err: AcceptanceError): List[Diff] = {
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

  implicit def seqComprator[A](implicit c: Comparator[A]): Comparator[Seq[A]] = new Comparator[Seq[A]] {
    def compare(left: Seq[A], right: Seq[A])(implicit err: AcceptanceError): List[Diff] = {
      if (left.size != right.size)
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

  def comparator2[A1,A2](names: (String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2],
                                                  err: AcceptanceError): Comparator[(A1,A2)] =
    new Comparator[(A1,A2)] {
      def compare(left: (A1,A2), right: (A1,A2))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2)
    }


  def comparator3[A1,A2,A3](names: (String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3],
                                                             err: AcceptanceError): Comparator[(A1,A2,A3)] =
    new Comparator[(A1,A2,A3)] {
      def compare(left: (A1,A2,A3), right: (A1,A2,A3))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3)
    }


  def comparator4[A1,A2,A3,A4](names: (String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4],
                                                                        err: AcceptanceError): Comparator[(A1,A2,A3,A4)] =
    new Comparator[(A1,A2,A3,A4)] {
      def compare(left: (A1,A2,A3,A4), right: (A1,A2,A3,A4))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4)
    }


  def comparator5[A1,A2,A3,A4,A5](names: (String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5],
                                                                                   err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5)] =
    new Comparator[(A1,A2,A3,A4,A5)] {
      def compare(left: (A1,A2,A3,A4,A5), right: (A1,A2,A3,A4,A5))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5)
    }


  def comparator6[A1,A2,A3,A4,A5,A6](names: (String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6],
                                                                                              err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6)] =
    new Comparator[(A1,A2,A3,A4,A5,A6)] {
      def compare(left: (A1,A2,A3,A4,A5,A6), right: (A1,A2,A3,A4,A5,A6))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6)
    }


  def comparator7[A1,A2,A3,A4,A5,A6,A7](names: (String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7],
                                                                                                         err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7), right: (A1,A2,A3,A4,A5,A6,A7))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7)
    }


  def comparator8[A1,A2,A3,A4,A5,A6,A7,A8](names: (String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8],
                                                                                                                    err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8), right: (A1,A2,A3,A4,A5,A6,A7,A8))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8)
    }


  def comparator9[A1,A2,A3,A4,A5,A6,A7,A8,A9](names: (String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9],
                                                                                                                               err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9)
    }


  def comparator10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](names: (String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10],
                                                                                                                                            err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10)
    }


  def comparator11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](names: (String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11],
                                                                                                                                                        err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11)
    }


  def comparator12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](names: (String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12],
                                                                                                                                                                    err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12)
    }


  def comparator13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](names: (String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13],
                                                                                                                                                                                err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13)
    }


  def comparator14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14],
                                                                                                                                                                                            err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14)
    }


  def comparator15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15],
                                                                                                                                                                                                        err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15)
    }


  def comparator16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15], comparator16: Comparator[A16],
                                                                                                                                                                                                                    err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15) ++
          comparator16.compareWithPath(names._16, left._16, right._16)
    }


  def comparator17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15], comparator16: Comparator[A16], comparator17: Comparator[A17],
                                                                                                                                                                                                                                err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15) ++
          comparator16.compareWithPath(names._16, left._16, right._16) ++
          comparator17.compareWithPath(names._17, left._17, right._17)
    }


  def comparator18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15], comparator16: Comparator[A16], comparator17: Comparator[A17], comparator18: Comparator[A18],
                                                                                                                                                                                                                                            err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15) ++
          comparator16.compareWithPath(names._16, left._16, right._16) ++
          comparator17.compareWithPath(names._17, left._17, right._17) ++
          comparator18.compareWithPath(names._18, left._18, right._18)
    }


  def comparator19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15], comparator16: Comparator[A16], comparator17: Comparator[A17], comparator18: Comparator[A18], comparator19: Comparator[A19],
                                                                                                                                                                                                                                                        err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15) ++
          comparator16.compareWithPath(names._16, left._16, right._16) ++
          comparator17.compareWithPath(names._17, left._17, right._17) ++
          comparator18.compareWithPath(names._18, left._18, right._18) ++
          comparator19.compareWithPath(names._19, left._19, right._19)
    }


  def comparator20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](names: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String))(implicit comparator1: Comparator[A1], comparator2: Comparator[A2], comparator3: Comparator[A3], comparator4: Comparator[A4], comparator5: Comparator[A5], comparator6: Comparator[A6], comparator7: Comparator[A7], comparator8: Comparator[A8], comparator9: Comparator[A9], comparator10: Comparator[A10], comparator11: Comparator[A11], comparator12: Comparator[A12], comparator13: Comparator[A13], comparator14: Comparator[A14], comparator15: Comparator[A15], comparator16: Comparator[A16], comparator17: Comparator[A17], comparator18: Comparator[A18], comparator19: Comparator[A19], comparator20: Comparator[A20],
                                                                                                                                                                                                                                                                    err: AcceptanceError): Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20)] =
    new Comparator[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20)] {
      def compare(left: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20), right: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20))(implicit err: AcceptanceError): List[Diff] =
        comparator1.compareWithPath(names._1, left._1, right._1) ++
          comparator2.compareWithPath(names._2, left._2, right._2) ++
          comparator3.compareWithPath(names._3, left._3, right._3) ++
          comparator4.compareWithPath(names._4, left._4, right._4) ++
          comparator5.compareWithPath(names._5, left._5, right._5) ++
          comparator6.compareWithPath(names._6, left._6, right._6) ++
          comparator7.compareWithPath(names._7, left._7, right._7) ++
          comparator8.compareWithPath(names._8, left._8, right._8) ++
          comparator9.compareWithPath(names._9, left._9, right._9) ++
          comparator10.compareWithPath(names._10, left._10, right._10) ++
          comparator11.compareWithPath(names._11, left._11, right._11) ++
          comparator12.compareWithPath(names._12, left._12, right._12) ++
          comparator13.compareWithPath(names._13, left._13, right._13) ++
          comparator14.compareWithPath(names._14, left._14, right._14) ++
          comparator15.compareWithPath(names._15, left._15, right._15) ++
          comparator16.compareWithPath(names._16, left._16, right._16) ++
          comparator17.compareWithPath(names._17, left._17, right._17) ++
          comparator18.compareWithPath(names._18, left._18, right._18) ++
          comparator19.compareWithPath(names._19, left._19, right._19) ++
          comparator20.compareWithPath(names._20, left._20, right._20)
    }

}