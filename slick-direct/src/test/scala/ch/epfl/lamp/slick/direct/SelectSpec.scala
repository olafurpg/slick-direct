package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class SelectSpec extends FlatSpec with TestHelper {

  "Query[T]" should "select *" in {
    // TODO: Require Query[T] to be inside query block
    val users = Query[User]
    equalQueries(
      queryDebug {
        users
      }.result,
      liftedUsers.result
    )
  }

}
