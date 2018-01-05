package com.github.hibou107.comparator



sealed trait Side
case object Left extends Side
case object Right extends Side

sealed trait ComparatorDiff

case class SizeDiff(left: Int, right: Int) extends ComparatorDiff
case class DoubleDiff(left: Double, right: Double) extends ComparatorDiff
case class KeyNotExist(key: String, side: Side) extends ComparatorDiff

case class Diff(paths: List[String], diff: ComparatorDiff)

case class AcceptanceError(relative: Double, absolute: Double)


trait Comparator[-A] { self =>
  def compare(left: A, right: A)(implicit err: AcceptanceError): List[Diff]

  def compareWithPath(path: String, left: A, right: A)(implicit err: AcceptanceError): List[Diff] = {
    val temp = compare(left, right)
    temp.map { diff => diff.copy(paths = path :: diff.paths)
    }
  }

  def contramap[B](f: B => A): Comparator[B] {
  } = new Comparator[B] {
    def compare(left: B, right: B)(implicit err: AcceptanceError): List[Diff] = self.compare(f(left), f(right))
  }
}

object Comparator {
  def compare[A](left: A, right: A)(implicit comparator: Comparator[A], acceptanceError: AcceptanceError): List[Diff] =
    comparator.compare(left, right)
}


object Test {
  import ComparatorImplicits._
  implicit val acceptanceError: AcceptanceError = AcceptanceError(1e-2, 1e-5)
  case class MyClass(first: Double, second: Double)
  val comp: Comparator[MyClass] = comparator2[Double, Double](("first", "second")).contramap(x => MyClass.unapply(x).get)
  def main(args: Array[String]): Unit = {
    println(comp.compare(MyClass(2, 2), MyClass(1, 3)))
  }
}