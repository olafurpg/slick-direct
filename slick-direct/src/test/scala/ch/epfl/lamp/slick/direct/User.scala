package ch.epfl.lamp.slick.direct

import org.scalatest.{ FlatSpec, FunSuite }
import slick.driver.H2Driver
import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.TypeTag


// Support is limited for now
case class UserId(value: Int) extends MappedTo[Int]
case class User(id: Int, name: String)


class Users(tag: Tag)
  extends Table[User](tag, "User") {

  def id: Rep[Int] = column[Int]("id")
  def name: Rep[String] = column[String]("name")

  def * = ProvenShape.proveShapeOf((id, name) <> ((User.apply _).tupled, User.unapply))
}

object UserId {
  implicit def id2UserId(i: Int): UserId = UserId(i)
}
