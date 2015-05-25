package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class FilterSpec extends FlatSpec with TestHelper {

  "filter" should "work with literal booleans" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.filter(u => true)
      }.result,
      liftedUsers.filter(u => u.id =!= 5).result
    )
  }

  it should "work with equality < condition" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.filter(u => u.id < 2)
      }.result,
      liftedUsers.filter(u => u.id < 2).result
    )
  }

  it should "work with equality > condition" in {
    val users = Query[User]
    equalQueries(
      queryDebug {
        users.filter(u => u.id > 1)
      }.result,
      liftedUsers.filter(u => u.id > 1).result
    )
  }

  // TODO: WIP
//  it should "work with equality == condition" in {
//    val users = Query[User]
//    equalQueries(
//      queryDebug {
//        users.filter(u => u.id == 1)
//      }.result,
//      liftedUsers.filter(u => columnExtensionMethods(u.id) === 1).result
//    )
//  }

}
