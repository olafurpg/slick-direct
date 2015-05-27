package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {
  "Query[T].map" should "work with string column" in {
    equalQueries(
      queryDebug {
        users.map(u => u.name)
      }.result,
      liftedUsers.map(u => u.name).result
    )
  }

  it should "work with int column" in {
    val d =
      query {
        users.map(u => u.id)
      }
    equalQueries(
      d.result,
      liftedUsers.map(u => u.id).result
    )
  }

  it should "work with string column extension methods" in {
    equalQueries(
      query {
        users.map(u => u.name + " Cool")
      }.result,
      // Note the ++, it's a caveat of slick.lifted.
      // One + will compile but give wrong results
      liftedUsers.map(u => u.name ++ " Cool").result
    )
  }

  it should "work with equality == condition for string" in {
    equalQueries(
      query {
        users.filter(u => u.name == "Olafur")
      }.result,
      liftedUsers.filter(u => u.name === "Olafur").result
    )
  }

  // TODO:
  //  MapSpec.scala:41: Typecheck during DSLVirtualization failed, overloaded method value infix_== with alternatives:
  //  [error]   (a: String,b: String)Boolean <and>
  //    [error]   (a: Int,b: Int)Boolean
  //    [error]  cannot be applied to (ch.epfl.lamp.slick.direct.UserId, ch.epfl.lamp.slick.direct.UserId)
  //    [error]         query {
  //    [error]               ^
//  it should "work with equality == condition for int" in {
//    equalQueries(
//      query {
//        users.filter(u => u.id == UserId(1))
//      }.result,
//      liftedUsers.filter(u => u.id === 1).result
//    )
//  }
  // TODO: Type rewrite for product types
  //    "Query[T].map" should "work with tuple selection" in {
  //      val users = Query[User]
  //      equalQueries(
  //        queryDebug {
  //          users.map(u => (u.name, u.id))
  //        }.result,
  //        liftedUsers.map(u => (u.name, u.id)).result
  //      )
  //    }

}
