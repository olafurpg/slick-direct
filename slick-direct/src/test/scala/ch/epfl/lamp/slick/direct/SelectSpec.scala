package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class SelectSpec extends FlatSpec with TestHelper {

  "Query[T]" should "select * from users" in {
    equalQueries(
      query {
        users
      }.result,
      liftedUsers.result
    )
  }

  "Query[T]" should "select * from cars" in {
    equalQueries(
      query {
        cars
      }.result,
      liftedCars.result
    )
  }
}
