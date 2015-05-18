package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].take" should "work" in {
    direct.Query.getTable[User]
    val users = Query[User]
//    MapQuery(users, (u: User) => direct.DslConfig.lift("name"))
//    equalQueries(
//      queryDebug {
//        users.map(_.name)
//      }.result,
//      liftedUsers.map(_.name).result
//    )
  }

}
