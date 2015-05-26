package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class FlatMapSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].flatMap" should "work with car.id select" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      query {
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

  "Query[T].flatMap" should "work with car.name select" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      query {
        for {
          user <- users
          car <- cars
        } yield car.name
      }.result,
      (for {
        user <- liftedUsers
        car <- liftedCars
      } yield car.name).result
    )
  }

  "Query[T].flatMap" should "work with filter" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      query {
        for {
          user <- users
          car <- cars if car.ownerId < 2
        } yield car.id
      }.result,
      (for {
        user <- liftedUsers
        car <- liftedCars if columnExtensionMethods(car.ownerId) < valueToConstColumn(2)
      } yield car.id).result
    )
  }

}
