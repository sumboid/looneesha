name := "looneesha"

version := "0.0.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"
