package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import slick.ast._
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class TakeSpec extends FlatSpec with TestHelper {
  // 1. Closure for reifyAs annotation
  // 2. Composition of queries
  // 3. Preprocessing for case classes

  "Query[T].take" should "work with take(1)" in {
    val users = Query[User]
    users.lift
    equalQueries(
      query {
        users.take(1)
      }.result,
      liftedUsers.take(1).result
    )
  }
  // TODO: Really strange compilation error
  it should "work with take(2).take(1)" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.take(2).take(1)
      }.result,
      liftedUsers.take(2).take(1).result
    )
  }
}
