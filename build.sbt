name := "scala-tests"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.7")