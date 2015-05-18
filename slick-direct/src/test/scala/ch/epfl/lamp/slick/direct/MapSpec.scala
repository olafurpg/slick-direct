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

  "Query[T].map" should "work with int column" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.map(u => u.id)
      }.result,
      liftedUsers.map(u => u.id).result
    )
  }

  // TODO: Problem with root driver.Table reference
//  "Query[T].map" should "work with string column extension methods" in {
//    val users = Query[User]
//    equalQueries(
//      queryDebug {
//        users.map(u => u.name + " Cool")
//      }.result,
//      liftedUsers.map(u => u.name + " Cool").result
//    )
//  }


  // TODO: Type rewrite for product types
  //  "Query[T].map" should "work with tuple selection" in {
  //    val users = Query[User]
  //    equalQueries(
  //      queryDebug {
  //        users.map(u => (u.name, u.id))
  //      }.result,
  //      liftedUsers.map(u => (u.name, u.id)).result
  //    )
  //  }

}
