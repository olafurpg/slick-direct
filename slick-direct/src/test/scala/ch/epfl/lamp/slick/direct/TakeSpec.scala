package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class TakeSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].take" should "work" in {
    direct.Query.getTable[User]
    val users = Query[User]
    
    equalQueries(
      queryDebug {
        users.take(1)
      }.result,
      liftedUsers.take(1).result
    )
  }

}
