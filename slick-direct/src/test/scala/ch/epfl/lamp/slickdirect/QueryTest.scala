package ch.epfl.lamp.slick.direct

import org.scalatest.FunSuite
import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape
import slick.profile.FixedSqlAction

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.TypeTag

trait TestHelper extends FunSuite {
  implicit def createQueryActionExtensionMethodsFromSlickQuery[T:TypeTag](q: SlickQuery[T]) = {
    slickDriver.createQueryActionExtensionMethods[Vector[User], NoStream](slickDriver.queryCompiler.run(q.toNode).tree, ())
  }
}

case class User(id: Int, name: String)

class Users(tag: Tag)
  extends Table[User](tag, "User") {

  def id: Rep[Int] = column[Int]("id", O.PrimaryKey)
  def name: Rep[String] = column[String]("name")

  def * = (id, name) <> (User.tupled, User.unapply)

}

class QueryTest extends FunSuite with TestHelper {
  val users = TableQuery[Users]

  test("Query.take should reify") {
    val db = Database.forConfig("h2mem1")
    try {
      val create: DBIO[Unit] = DBIO.seq(
        users.schema.create,
        users ++= Seq(
          User(1, "Olafur"),
          User(2, "Vojin")
        )
      )
      val setup: Future[Unit] = db.run(create)

      val f: Future[Boolean] = setup.flatMap { _ =>
        for {
          direct <- db.run(query {
            Query[User].take(1)
          }.result)
          lifted <- db.run(users.take(1).result)
        } yield direct == lifted
      }
      assert(Await.result(f, Duration.Inf) === true)
    }
    finally db.close()
  }
}
