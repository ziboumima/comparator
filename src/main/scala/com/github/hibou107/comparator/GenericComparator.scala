package com.github.hibou107.comparator

import shapeless.labelled.FieldType
import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy, Witness }
import shapeless.syntax.std.tuple._

object GenericComparator {

  implicit val hNilComparator: Comparator[HNil] = new Comparator[HNil] {
    override def compare(left: HNil, right: HNil)(implicit err: AcceptanceError): List[Diff] = Nil
  }

  implicit def hHlistFieldComparator[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Comparator[H]],
    tEncoder: Comparator[T]): Comparator[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    new Comparator[FieldType[K, H] :: T] {
      override def compare(left: FieldType[K, H] :: T, right: FieldType[K, H] :: T)(implicit err: AcceptanceError) = {
        val headDiffs = hEncoder.value.compare(left.head, right.head)
        val tailDiffs = tEncoder.compare(left.tail, right.tail)
        val headWithPath = headDiffs.map(d => d.copy(paths = fieldName :: d.paths))
        headWithPath ++ tailDiffs
      }
    }

  }

  implicit def genericObjectComparator[A, H](implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Comparator[H]]): Comparator[A] =
    new Comparator[A] {
      override def compare(left: A, right: A)(implicit err: AcceptanceError): List[Diff] = {
        hEncoder.value.compare(generic.to(left), generic.to(right))
      }
    }

}
