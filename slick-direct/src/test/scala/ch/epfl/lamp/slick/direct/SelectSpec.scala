package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class SelectSpec extends FlatSpec with TestHelper {
  // 0. Stick AST into Query[T]
  // 1. Composition of queries
  // 2. Closure for reifyAs annotation
  // 3. Preprocessing for case classes

  "Query[T]" should "select *" in {
    direct.Query.getTable[User]
    equalQueries(
      Query[User].result,
      liftedUsers.result
    )
  }

}
