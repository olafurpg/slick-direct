import sbt._

object Dependencies {
  val dbDeps = Seq(
    "com.typesafe.slick" %% "slick" % "3.0.0", "com.h2database" % "h2" % "1.3.175"
  )

  val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )

  val slickDirect = dbDeps ++ testDeps ++ Seq(
  )
}
