name := "Scala"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq( "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.3.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0")
