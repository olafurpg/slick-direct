package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class ProblemSpec extends FlatSpec with TestHelper {

  "Query.flatMap" should "work" in {
    val users = Query[User]
    val cars = Query[User]
    val direct =
      queryDebug {
        for {
          user <- users
          car <- users
        } yield user.name
      }

    equalQueries(
      direct.result,
      (for {
        user <- liftedUsers
        car <- liftedCars
      } yield user.name).result
    )
  }

//  "Queries" should "compose" in {
//    val users = Query[User]
//    val cars = Query[User]
//    val direct1 = query {
//      users.map(_.name)
//    }
//    val direct = query {
//      for {
//        name <- direct1
//      } yield name
//    }
//    equalQueries(
//      direct.result,
//      (for {
//        user <- liftedUsers
//        car <- liftedCars
//      } yield (user.name, car.name)).result
//    )
//  }

}
