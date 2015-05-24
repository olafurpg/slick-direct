package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class ProblemSpec extends FlatSpec with TestHelper {
  val cars = Query[User]
      // Closure for reifyAs annotation
      // Composition of queries (stick AST inside Query[T])
      // Preprocessing for case classes


    "Query" should "work with multiple take" in {
      val users = Query[User]
      import ch.epfl.lamp.slick.direct.Config._;
      val k = ch.epfl.lamp.slick.direct.Config.queryLift(users).take(ch.epfl.lamp.slick.direct.Config.constColumnLift(2L)).take(ch.epfl.lamp.slick.direct.Config.constColumnLift(1L))
      println(k)
    }
}
