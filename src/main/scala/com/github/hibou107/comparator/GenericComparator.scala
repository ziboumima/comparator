package com.github.hibou107.comparator

import shapeless.labelled.FieldType
import shapeless.{ :+:, ::, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness }

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
      override def compare(left: FieldType[K, H] :: T, right: FieldType[K, H] :: T)(implicit err: AcceptanceError): List[Diff] = {
        val headDiffs = hEncoder.value.compare(left.head, right.head)
        val tailDiffs = tEncoder.compare(left.tail, right.tail)
        val headWithPath = headDiffs.map(d => d.copy(paths = fieldName :: d.paths))
        headWithPath ++ tailDiffs
      }
    }
  }

  implicit def coProductComparator[H, T <: Coproduct](implicit hComparator: Lazy[Comparator[H]], tComparator: Comparator[T]): Comparator[H :+: T] =
    new Comparator[:+:[H, T]] {
      def compare(left: :+:[H, T], right: :+:[H, T])(implicit err: AcceptanceError): List[Diff] = {
        (left, right) match {
          case (Inl(h1), Inl(h2)) => hComparator.value.compare(h1, h2)
          case (Inr(h1), Inr(h2)) => tComparator.compare(h1, h2)
          case (x, y)             => Diff(Nil, TypeDiff(x.toString, y.toString)) :: Nil
        }
      }
    }

  implicit def genericComparator[A, H](implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Comparator[H]]): Comparator[A] =
    new Comparator[A] {
      override def compare(left: A, right: A)(implicit err: AcceptanceError): List[Diff] = {
        hEncoder.value.compare(generic.to(left), generic.to(right))
      }
    }

}
