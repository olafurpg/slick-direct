import sbt._

object Dependencies {
  val dbDeps = Seq(
    "com.typesafe.slick" %% "slick" % "3.0.0"
    , "com.h2database" % "h2" % "1.3.175"
  )

  val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
    , "com.typesafe.slick" %% "slick-testkit" % "3.0.0" % "test"
  )

  val slickDirect = dbDeps ++ testDeps
}
