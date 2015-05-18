package ch.epfl.lamp.slick.direct

import org.scalatest.{ FlatSpec, FunSuite }
import slick.driver.H2Driver
import slick.driver.H2Driver.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

trait TestHelper extends FlatSpec {
  val liftedUsers = TableQuery[Users]
  val liftedCars = TableQuery[Cars]

  type DB = H2Driver.backend.DatabaseDef

  def withDb[T](testCode: DB => Future[(T, T)]): Unit = {
    val db = Database.forConfig("h2mem1")
    try {
      val create: DBIO[Unit] = DBIO.seq(
        liftedUsers.schema.create,
        liftedCars.schema.create,
        liftedUsers ++= Seq(
          User(1, "Olafur"),
          User(2, "Vojin")
        ),
        liftedCars ++= Seq(
          Car(1, "Ford Taurus", 1),
          Car(2, "Auris", 1)
        )
      )
      val f = for {
        setup <- db.run(create)
        result <- testCode(db)
      } yield result
      val (expected, obtained) = Await.result(f, Duration.Inf)
      assert(expected === obtained)
    }
    finally db.close()
  }

  def equalQueries[T](directQ: DBIO[T], liftedQ: DBIO[T]): Unit = withDb { db =>
    for {
      direct <- db.run(directQ)
      lifted <- db.run(liftedQ)
    } yield (direct, lifted)
  }
}
