package ch.epfl.lamp.slick.direct

import slick.driver.JdbcDriver
import slick.{ ast => sq }

import scala.reflect.runtime.universe.TypeTag

sealed trait SlickQuery[T] extends Query[T] {
  def toNode(implicit driver: JdbcDriver, typetag: TypeTag[T]): sq.Node = {
    new SlickQueryMapper(driver, typetag).toNode(this)
  }
}

case class Const[T](e: T) extends SlickQuery[T]
case class Select_*[T]() extends SlickQuery[T]
case class Take[T](self: SlickQuery[_], n: Const[Int]) extends SlickQuery[T]
case class Drop[T](self: SlickQuery[T], n: Const[Int]) extends SlickQuery[T]
