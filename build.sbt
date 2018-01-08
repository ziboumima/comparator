import scalariform.formatter.preferences._

name := "comparator"

version := "0.1"

scalaVersion := "2.12.4"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.18"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)