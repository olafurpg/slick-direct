package ch.epfl.lamp.slick.direct
import slick.driver.JdbcDriver

import scala.reflect.runtime.universe._
import slick.{ ast => sq }

trait DirectSlickModule {
  val driver: JdbcDriver
}

