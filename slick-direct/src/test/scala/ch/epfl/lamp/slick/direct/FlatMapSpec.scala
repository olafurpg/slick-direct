package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class FlatMapSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].flatMap" should "work without filter" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      queryDebug {
        for {
          user <- users
          car <- cars
        } yield car.id
      }.result,
      (for {
        user <- liftedUsers
        car <- liftedCars
      } yield car.id).result
    )
  }
  // TODO: Type rewrite for product types
  //  "Query[T].map" should "work with tuple selection" in {
  //    val users = Query[User]
  //    equalQueries(
  //      queryDebug {
  //        users.map(u => (u.name, u.id))
  //      }.result,
  //      liftedUsers.map(u => (u.name, u.id)).result
  //    )
  //  }

}
