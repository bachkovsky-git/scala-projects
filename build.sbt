name := "scala-tests"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
// https://mvnrepository.com/artifact/net.sf.sociaal/freetts
libraryDependencies += "net.sf.sociaal" % "freetts" % "1.2.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test
)
// https://mvnrepository.com/artifact/com.google.code.gson/gson
libraryDependencies += "com.google.code.gson" % "gson" % "2.6.2"
