package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class SelectSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries (stick AST inside Query[T])
  // 3. Preprocessing for case classes

  "Query[T]" should "select *" in {
    direct.Query.getTable[User]
    equalQueries(
      Query[User].result,
      liftedUsers.result
    )
  }

}
