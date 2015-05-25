package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class FilterSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "filter" should "work with literal booleans" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.filter(u => true)
      }.result,
      liftedUsers.filter(u => u.id =!= 5).result
    )
  }

  it should "work with equality == condition"{
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.filter(u => true)
      }.result,
      liftedUsers.filter(u => u.id =!= 5).result
    )
  }

}
