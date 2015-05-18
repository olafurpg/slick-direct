package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {

  "Query.map" should "reify on User.name" in {
    val dUsers = Query[User]
    equalQueries(
      query {
        for {
          user <- dUsers
        } yield user.name
      }.result,
      liftedUsers.map(_.name).result
    )
  }

  it should "reify on User.id" in {
    val dUsers = Query[User]
    equalQueries(
      query {
        dUsers.map(u => u.id)
      }.result,
      liftedUsers.map(_.id).result
    )
  }

  it should "reify on Car.id" in {
    val dCars = Query[Car]
    equalQueries(
      query {
        dCars.map(u => u.id)
      }.result,
      liftedCars.map(_.id).result
    )
  }

}
