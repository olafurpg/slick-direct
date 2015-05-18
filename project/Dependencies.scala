import sbt._

object Dependencies {
  val dbDeps = Seq(
    "com.typesafe.slick" %% "slick" % "3.0.0", "com.h2database" % "h2" % "1.3.175"
  )

  val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "com.typesafe.slick" %% "slick" % "3.0.0",
    "com.typesafe.slick" %% "slick-testkit" % "3.0.0" % "test",
    "com.novocode" % "junit-interface" % "0.10" % "test",
    "ch.qos.logback" % "logback-classic" % "0.9.28" % "test",
    "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test"
  )

  val slickDirect = dbDeps ++ testDeps ++ Seq(
    "org.slf4j" % "slf4j-api" % "1.7.12"
  )
}
