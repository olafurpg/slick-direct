name := "slick-direct"

organization := "ch.epfl.lamp"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

lazy val root = (project in file(".")).aggregate(slickDirect)

lazy val slickDirect = (project in file("slick-direct")).settings(
        scalaVersion := "2.11.6",
        libraryDependencies ++= Dependencies.slickDirect,
        scalacOptions ++= Seq("-language:experimental.macros"),
        testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
        parallelExecution in Test := false,
        javaOptions += "-Xmx4G",
        logBuffered := false,
          testOptions in Test += Tests.Argument("-oF")
        ).dependsOn(directembedding)

lazy val directembedding = ProjectRef(file("./directembedding"), "directembedding")


initialCommands := "import ch.epfl.lamp.slickdirect._"

