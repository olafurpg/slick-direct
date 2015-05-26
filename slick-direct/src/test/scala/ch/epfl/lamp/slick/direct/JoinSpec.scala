package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class JoinSpec extends FlatSpec with TestHelper {

  "crossJoin" should "work" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      queryDebug {
        users join cars
      }.result,
      (liftedUsers join liftedCars).result
    )
  }

  it should "work with on" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      query {
        users join cars on (_.id == _.ownerId)
      }.result,
      (liftedUsers join liftedCars on (_.id === _.ownerId)).result
    )
  }

}
