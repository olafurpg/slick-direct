package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import slick.ast._
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class TakeSpec extends FlatSpec with TestHelper {

  "Query[T].take" should "work with take(1)" in {
    equalQueries(
      query {
        users.take(1)
      }.result,
      liftedUsers.take(1).result
    )
  }

  it should "work with take(2).take(1)" in {
    val users = Query[User]
    equalQueries(
      query {
        users.take(2).take(1)
      }.result,
      liftedUsers.take(2).take(1).result
    )
  }
}
