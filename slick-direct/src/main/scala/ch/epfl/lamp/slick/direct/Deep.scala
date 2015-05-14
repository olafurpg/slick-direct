package ch.epfl.lamp.slick.direct

import slick.driver.JdbcDriver
import slick.{ ast => sq }

import scala.reflect.runtime.universe.TypeTag

sealed trait SlickQuery[T] extends Query[T] {
  def toNode(implicit driver: JdbcDriver): sq.Node = {
    new SlickQueryMapper(driver).toNode(this)
  }

  def getTypeTag: Option[TypeTag[_]]
}

case class Const[T](e: T) extends SlickQuery[T] {
  lazy val getTypeTag: Option[TypeTag[_]] = e match {
    case BaseQuery(tt) => Some(tt)
    case _ => None
  }
}

case class Select_*[T](tt: TypeTag[T]) extends SlickQuery[T] {
  lazy val getTypeTag: Option[TypeTag[_]] = Some(tt)
}

case class Take[T](self: SlickQuery[_], n: Const[Int]) extends SlickQuery[T] {
  lazy val getTypeTag: Option[TypeTag[_]] = self.getTypeTag
}

// TODO: What about multiple columns, make fields: List[String]?
case class MapAction[U](self: SlickQuery[_], field: String) extends SlickQuery[U] {
  lazy val getTypeTag: Option[TypeTag[_]] = self.getTypeTag
}
