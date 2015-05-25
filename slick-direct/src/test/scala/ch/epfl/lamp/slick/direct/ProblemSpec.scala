package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class ProblemSpec extends FlatSpec with TestHelper {
  val cars = Query[User]
      // Closure for reifyAs annotation
      // Composition of queries (stick AST inside Query[T])
      // Preprocessing for case classes


  // TODO: This doesn't compile after the macro expansion, although it compiles here :/
    "Query" should "work with multiple take" in {
      val users = Query[User]
      import ch.epfl.lamp.slick.direct.Config._;
      val i = ch.epfl.lamp.slick.direct.Config.compile({
        import ch.epfl.directembedding.transformers.RewireEmbeddedControls._;
        {
          import ch.epfl.lamp.slick.direct.Config._;
          ch.epfl.lamp.slick.direct.Config.queryLift(users).take(ch.epfl.lamp.slick.direct.Config.constColumnLift(2L)).take(ch.epfl.lamp.slick.direct.Config.constColumnLift(1L))
        }
      })
      println(i)
    }
  "+" should "work on string" in {

    val users = TableQuery[Users]
    val res = stringColumnExtensionMethods(SlickReification.column[ch.epfl.lamp.slick.direct.User, String](users.baseTableRow, "name", "java.lang.String")) ++ (" Cool")
    println(res)
  }
}
