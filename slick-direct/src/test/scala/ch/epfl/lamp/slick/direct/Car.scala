package ch.epfl.lamp.slick.direct

import org.scalatest.{ FlatSpec, FunSuite }
import slick.driver.H2Driver
import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.TypeTag

case class Car(id: Int, name: String, ownerId: UserId)

class Cars(tag: Tag)
  extends Table[Car](tag, "Car") {

  def id: Rep[Int] = column[Int]("id", O.PrimaryKey)
  def name: Rep[String] = column[String]("name")
  def ownerId: Rep[UserId] = column[UserId]("ownerId")

  def * = ProvenShape.proveShapeOf((id, name, ownerId) <> (Car.tupled, Car.unapply))
}
