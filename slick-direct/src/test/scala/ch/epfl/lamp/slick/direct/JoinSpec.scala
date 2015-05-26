package ch.epfl.lamp.slick.direct

import org.scalatest.FlatSpec
import slick.driver.H2Driver.api._

class JoinSpec extends FlatSpec with TestHelper {

  "crossJoin" should "work" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      query {
        users join cars
      }.result,
      (liftedUsers join liftedCars).result
    )
  }

  "innerJoin" should "work" in {
    val users = Query[User]
    val cars = Query[Car]
    equalQueries(
      queryDebug {
        users join cars on (_.id == _.ownerId)
      }.result,
      (liftedUsers join liftedCars on (_.id === _.ownerId)).result
    )
  }

  "fullOuterJoin" should "work" in {
    val users = Query[User]
    val cars = Query[Car]
    val d = queryDebug {
      users joinFull cars on (_.id == _.ownerId)
    }
    equalQueries(
      d.result,
      (liftedUsers joinFull liftedCars on (_.id === _.ownerId)).result
    )
  }

  // TODO: We need to solve this dsl.compile mess
  // This will need to expand to arbitrarily nested types
//    [error] /Users/ollie/Dropbox/epfl/2014-2015/spring/lamp/slick-direct/slick-direct/src/test/scala/ch/epfl/lamp/slick/direct/JoinSpec.scala:38: type mismatch;
//  [error]  found   : ch.epfl.lamp.slick.direct.BaseJoinQuery[slick.lifted.AbstractTable[ch.epfl.lamp.slick.direct.User]#TableElementType,Option[ch.epfl.lamp.slick.direct.Car],slick.lifted.AbstractTable[ch.epfl.lamp.slick.direct.User]#TableElementType,Option[ch.epfl.lamp.slick.direct.Car],Seq]
//  [error]  required: ch.epfl.lamp.slick.direct.Query[(ch.epfl.lamp.slick.direct.User, Option[ch.epfl.lamp.slick.direct.Car]),Seq]
//  [error]       val d = queryDebug {
//    [error]                          ^
//    "leftOuterJoin" should "work" in {
//      val users = Query[User]
//      val cars = Query[Car]
//      val d = queryDebug {
//        users joinLeft cars on (_.id == _.ownerId)
//      }
//      equalQueries(
//        d.result,
//        (liftedUsers joinLeft liftedCars).result
//      )
//    }

//    "rightOuterJoin" should "work" in {
//      val users = Query[User]
//      val cars = Query[Car]
//      val d = queryDebug {
//        users joinRight cars on (_.id == _.ownerId)
//      }
//      equalQueries(
//        d.result,
//        (liftedUsers joinRight liftedCars).result
//      )
//    }

  // TODO: [error] /Users/ollie/Dropbox/epfl/2014-2015/spring/lamp/slick-direct/slick-direct/src/test/scala/ch/epfl/lamp/slick/direct/JoinSpec.scala:52: Unable to resolve type for (_1: ch.epfl.lamp.slick.direct.User, _2: Option[ch.epfl.lamp.slick.direct.Car])(ch.epfl.lamp.slick.direct.User, Option[ch.epfl.lamp.slick.direct.Car])((u @ _), (c @ _))
  //    [error]           (u, c) <- users joinLeft cars on (_.id == _.ownerId)
  //    [error]           ^
  //    "leftOuterJoin" should "work" in {
  //      val users = Query[User]
  //      val cars = Query[Car]
  //      val d = queryDebug {
  //        for {
  //          (u, c) <- users joinLeft cars on (_.id == _.ownerId)
  //        } yield u.name
  //      }
  //      val l = for  {
  //        (u, c) <- liftedUsers joinLeft liftedCars
  //      } yield u.name
  //
  //      equalQueries(
  //        d.result,
  //        l.result
  //      )
  //    }

}
