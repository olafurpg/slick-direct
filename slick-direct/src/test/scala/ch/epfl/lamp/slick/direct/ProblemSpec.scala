package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class ProblemSpec extends FlatSpec with TestHelper {

  // TODO: WIP
  "leftOuterJoin" should "work" in {
    val users = Query[User]
    val cars = Query[Car]
    val d =
      queryDebug {
        users joinLeft cars on (_.id == _.map(_.id).getOrElse(-1))
      }
    equalQueries(
      d.result,
      (liftedUsers joinLeft liftedCars).result
    )
  }
}
