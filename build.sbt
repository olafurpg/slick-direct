name := "slick-direct"

organization := "ch.epfl.lamp"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
)

initialCommands := "import ch.epfl.lamp.slickdirect._"

