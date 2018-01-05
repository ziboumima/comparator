package com.github.hibou107.Comparator

object Generator {
  private def tupleGenerator(size: Int) = {
    {(1 to size).map(n => s"A$n").mkString(",")}
  }
  def main(args: Array[String]): Unit = {

  }
}
