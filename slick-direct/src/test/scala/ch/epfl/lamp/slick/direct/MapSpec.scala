package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].map" should "work" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.map(u => u.name)
      }.result,
      liftedUsers.map(u => u.id).result
    )
  }

}
