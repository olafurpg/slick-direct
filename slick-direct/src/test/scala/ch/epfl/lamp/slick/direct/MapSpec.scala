package ch.epfl.lamp.slick.direct

import ch.epfl.lamp.slick.direct
import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class MapSpec extends FlatSpec with TestHelper {

  "Query[T].map" should "work with string column" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.map(u => u.name)
      }.result,
      liftedUsers.map(u => u.name).result
    )
  }

  it should "work with int column" in {
    val users = Query[User]
    equalQueries(
      query {
        users.map(u => u.id)
      }.result,
      liftedUsers.map(u => u.id).result
    )
  }

  it should "work with string column extension methods" in {
    val users = Query[User]
    equalQueries(
      query {
        users.map(u => u.name + " Cool")
      }.result,
      // Note the ++, it's a caveat of slick.lifted.
      // One + will compile but give wrong results
      liftedUsers.map(u => u.name ++ " Cool").result
    )
  }

  it should "work with equality == condition for int" in {
    val users = Query[User]
    equalQueries(
      query {
        users.filter(u => u.id == 1)
      }.result,
      liftedUsers.filter(u => u.id === 1).result
    )
  }

  it should "work with equality == condition for string" in {
    val users = Query[User]
    equalQueries(
      query {
        users.filter(u => u.name == "Olafur")
      }.result,
      liftedUsers.filter(u => u.name === "Olafur").result
    )
  }

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
