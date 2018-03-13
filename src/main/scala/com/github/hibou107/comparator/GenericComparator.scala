package com.github.hibou107.comparator

import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

object GenericComparator {

  implicit val hNilComparator: Comparator[HNil] = (left: HNil, right: HNil) => Nil

  implicit val cNilComparator: Comparator[CNil] = (left: CNil, right: CNil) => Nil

  implicit def hlistFieldComparator[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Comparator[H]],
    tEncoder: Comparator[T]): Comparator[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    (left: FieldType[K, H] :: T, right: FieldType[K, H] :: T) => {
      val headDiffs = hEncoder.value.compare(left.head, right.head)
      val tailDiffs = tEncoder.compare(left.tail, right.tail)
      val headWithPath = headDiffs.map(d => d.copy(paths = fieldName :: d.paths))
      headWithPath ++ tailDiffs
    }
  }

  implicit def coProductComparator[K <: Symbol, H, T <: Coproduct](
                                                                    implicit
                                                                    witness: Witness.Aux[K],
                                                                    hComparator: Lazy[Comparator[H]],
                                                                    tComparator: Comparator[T]
                                                               ): Comparator[FieldType[K, H] :+: T] =
      (left: FieldType[K, H] :+: T, right: FieldType[K, H] :+: T) => {
        (left, right) match {
          case (Inl(h1), Inl(h2)) => hComparator.value.compare(h1, h2)
          case (Inr(h1), Inr(h2)) => tComparator.compare(h1, h2)
          case (x, y)             => Diff(Nil, TypeDiff(x.toString, y.toString)) :: Nil
        }
      }

  implicit def coProductComparator[H, T <: Coproduct](implicit hComparator: Lazy[Comparator[H]], tComparator: Comparator[T]): Comparator[H :+: T] =
    (left: :+:[H, T], right: :+:[H, T]) => {
      (left, right) match {
        case (Inl(h1), Inl(h2)) => hComparator.value.compare(h1, h2)
        case (Inr(h1), Inr(h2)) => tComparator.compare(h1, h2)
        case (x, y)             => Diff(Nil, TypeDiff(x.toString, y.toString)) :: Nil
      }
    }
  implicit def genericComparator[A, H](implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Comparator[H]]): Comparator[A] =
    (left: A, right: A) => {
      hEncoder.value.compare(generic.to(left), generic.to(right))
    }

}
