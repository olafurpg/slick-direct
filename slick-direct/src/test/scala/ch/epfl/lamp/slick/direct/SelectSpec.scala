package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class SelectSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T]" should "select *" in {
//    direct.Query.getTable[User]
//    val users = Query[User]
//    queryDebug {
//      users
//    }.lift.result
    equalQueries(
      Query[User].lift.result,
      liftedUsers.result
    )
  }

}
