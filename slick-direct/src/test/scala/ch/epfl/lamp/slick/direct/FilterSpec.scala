package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class FilterSpec extends FlatSpec with TestHelper {

//  "filter" should "work with literal booleans" in {
//    val users = Query[User]
//    equalQueries(
//      query {
//        users.filter(u => true)
//      }.result,
//      liftedUsers.filter(u => u.id =!= 5).result
//    )
//  }
//
//  it should "work with equality < condition" in {
//    val users = Query[User]
//    equalQueries(
//      query {
//        users.filter(u => u.id < 2)
//      }.result,
//      liftedUsers.filter(u => columnExtensionMethods(u.id) < valueToConstColumn(2)).result
//    )
//  }
//
//  it should "work with equality > condition" in {
//    val users = Query[User]
//    equalQueries(
//      query {
//        users.filter(u => u.id > 1)
//      }.result,
//      liftedUsers.filter(u => u.id > 1).result
//    )
//  }
//
//  "filterNot" should "work with literal booleans" in {
//    val users = Query[User]
//    equalQueries(
//      query {
//        users.filterNot(u => true)
//      }.result,
//      liftedUsers.filterNot(u => u.id =!= 5).result
//    )
//  }
//
//  "withFilter" should "work with literal booleans" in {
//    val users = Query[User]
//    equalQueries(
//      query {
//        users.withFilter(u => true)
//      }.result,
//      liftedUsers.withFilter(u => u.id =!= 5).result
//    )
//  }

}
