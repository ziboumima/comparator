package com.github.hibou107.comparator

sealed trait Side
case object Left extends Side
case object Right extends Side

sealed trait ComparatorDiff

final case class SizeDiff(left: Int, right: Int) extends ComparatorDiff
final case class DoubleDiff(left: Double, right: Double) extends ComparatorDiff
final case class StringDiff(left: String, right: String) extends ComparatorDiff
final case class KeyNotExist(key: String, side: Side) extends ComparatorDiff
final case class TypeDiff(left: String, right: String) extends ComparatorDiff
case class Diff(paths: List[String], diff: ComparatorDiff)

case class AcceptanceError(relative: Double, absolute: Double)

trait Comparator[A] { self =>
  def compare(left: A, right: A): List[Diff]

  def compareWithPath(path: String, left: A, right: A): List[Diff] = {
    val temp = compare(left, right)
    temp.map { diff => diff.copy(paths = path :: diff.paths)
    }
  }

  def contramap[B](f: B => A): Comparator[B] {
  } = new Comparator[B] {
    def compare(left: B, right: B): List[Diff] = self.compare(f(left), f(right))
  }
}

object Comparator {
  private def renderDiff(diffs: List[Diff]): String = {
    val temp = diffs map { diff =>
      val pathStr = diff.paths.mkString(" / ")
      val result = diff.diff match {
        case SizeDiff(left, right)   => s"left.size($left) != right.size($right)"
        case DoubleDiff(left, right) => s"$left != $right"
        case StringDiff(left, right) => s"$left != $right"
        case KeyNotExist(key, side)  => s"key $key does not exist on the $side"
        case TypeDiff(left, right)   => s"type is not the same ($left and $right)"
      }
      s"$pathStr / $result"
    }
    temp.mkString("\n")
  }
  class DiffAssertion(diff: String) extends Throwable

  def apply[A](implicit c: Comparator[A]): Comparator[A] = c
  def compare[A](left: A, right: A)(implicit comparator: Comparator[A], acceptanceError: AcceptanceError): List[Diff] =
    comparator.compare(left, right)

  def assertEqual[A](left: A, right: A)(implicit comparator: Comparator[A], acceptanceError: AcceptanceError): Unit = {
    val diffs = Comparator.compare(left, right)
    if (diffs.nonEmpty) {
      val str = renderDiff(diffs)
      println(str)
      throw new DiffAssertion(str)
    }

  }

}
