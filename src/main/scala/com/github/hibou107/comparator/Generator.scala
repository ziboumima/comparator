package com.github.hibou107.comparator

object Generator {
  private def tupleGenerator(size: Int) = {
    { (1 to size).map(n => s"A$n").mkString(",") }
  }

  private def strGenerator(index: Int) = {
    List.fill(index + 1)("String").mkString(", ")
  }

  private def comparatorStrGenerator(size: Int) = {
    (1 to size).map { n =>
      s"comparator$n: Comparator[A$n]"
    }.mkString(", ")
  }

  private def funcGenerator(size: Int) = {
    (1 to size).map { n =>
      s"comparator$n.compareWithPath(names._$n, left._$n, right._$n)"
    }.mkString(" ++\n")
  }

  private def generateOne(n: Int): String = {
    val tupled = tupleGenerator(n)
    s"""|def comparator$n[$tupled](names: (${strGenerator(n)}))(implicit ${comparatorStrGenerator(n)},
          |                                                   err: AcceptanceError): Comparator[($tupled)] =
          |    new Comparator[($tupled)] {
          |      def compare(left: ($tupled), right: ($tupled))(implicit err: AcceptanceError): List[Diff] =
          |         ${funcGenerator(n)}
          |    }
      """.stripMargin

  }

  private def generate(from: Int, to: Int): String = {
    (from to to).map { n =>
      generateOne(n)
    }.mkString("\n\n")

  }

  def main(args: Array[String]): Unit = {
    val x = generate(2, 20)
    println(x)

  }
}
