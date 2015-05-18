package ch.epfl.lamp.slick.direct

import org.scalatest.{ FlatSpec }
import slick.driver.H2Driver.api._

class TakeSpec extends FlatSpec with TestHelper {
  "Query.take" should "reify on User.take(0)" in {
    // TODO: value dUsers on class QueryTest is not supported in slick-direct
    val dUsers = Query[User]

    equalQueries(
      query {
        dUsers.take(0)
      }.result,
      liftedUsers.take(0).result
    )
  }

  it should "reify on User.take(1)" in {
    val dUsers = Query[User]
    equalQueries(
      query {
        dUsers.take(1)
      }.result,
      liftedUsers.take(1).result
    )
  }

  it should "reify on Car.take(2)" in {
    val dCars = Query[Car]
    equalQueries(
      query {
        dCars.take(1)
      }.result,
      liftedCars.take(1).result
    )
  }

}
