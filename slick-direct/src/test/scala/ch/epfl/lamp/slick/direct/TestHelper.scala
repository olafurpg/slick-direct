package ch.epfl.lamp.slick.direct

import org.scalatest.{ FlatSpec, FunSuite }
import slick.driver.H2Driver
import slick.driver.H2Driver.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

trait TestHelper extends FlatSpec {
  val users = TableQuery[Users]
  val cars = TableQuery[Cars]

  type DB = H2Driver.backend.DatabaseDef

  def withDb(testCode: DB => Future[Boolean]): Unit = {
    val db = Database.forConfig("h2mem1")
    try {
      val create: DBIO[Unit] = DBIO.seq(
        users.schema.create,
        cars.schema.create,
        users ++= Seq(
          User(1, "Olafur"),
          User(2, "Vojin")
        ),
        cars ++= Seq(
          Car(1, "Ford Taurus", 1),
          Car(2, "Auris", 1)
        )
      )
      val f = for {
        setup <- db.run(create)
        result <- testCode(db)
      } yield result
      assert(Await.result(f, Duration.Inf) === true)
    }
    finally db.close()
  }

  def equalQueries[T](directQ: DBIO[T], liftedQ: DBIO[T]): Unit = withDb { db =>
    for {
      direct <- db.run(directQ)
      lifted <- db.run(liftedQ)
    } yield direct == lifted
  }
}
