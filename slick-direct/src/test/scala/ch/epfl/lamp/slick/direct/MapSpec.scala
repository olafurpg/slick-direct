package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {

  "Query.map" should "reify on User.name" in {
    val dUsers = Query[User]
    equalQueries(
      query {
        dUsers.map(u => u.name)
      }.result,
      users.map(_.name).result
    )
  }

  it should "reify on User.id" in {
    val dUsers = Query[User]
    equalQueries(
      query {
        dUsers.map(u => u.id)
      }.result,
      users.map(_.id).result
    )
  }

  it should "reify on Car.id" in {
    val dCars = Query[Car]
    equalQueries(
      query {
        dCars.map(u => u.id)
      }.result,
      cars.map(_.id).result
    )
  }

}
