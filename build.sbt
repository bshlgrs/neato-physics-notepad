name := "scala-gem"

version := "0.1"

enablePlugins(ScalaJSPlugin)

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.0",
)

resolvers ++= List(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)
