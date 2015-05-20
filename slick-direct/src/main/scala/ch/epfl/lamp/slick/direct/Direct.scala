package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
import slick.ast
import slick.ast._
import slick.driver.JdbcDriver
import slick.lifted._
import slick.model.{ Table, Column, QualifiedName }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }

trait Query[T] {
  /**
   * The accumulated AST in this query
   */
  def lift: slick.lifted.QueryBase[_]
  type Self = T
  // TODO: add shape to query

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???

  def map(f: T => String): Query[String] = ???

  def flatMap[U](f: T => Query[U]): Query[U] = ???
}


object SlickReification extends SlickReflectUtil {
//  def map(lhs: Query[_], f: Function1[_, _]): Node = {
//    val q = new Query[String] {
//      def ast = lhs.toNode
//      def lift = ???
//      type Self = String
//    }
//    new slick.ast.Bind(new AnonSymbol, lhs.toNode, q.toNode)
//  }

}

object Query extends SlickReflectUtil {

  def getTable[T: TypeTag]: slick.model.Table = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    println(table)
    table
  }

  @reifyAs(TableExpansion)
  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[T] = {
    val table = getTable[T]
    new Query[T] {
      class LiftedTable(tag: Tag) extends driver.Table[T](tag, table.name.table) {
        def * = ??? // We override toNode
      }

      def lift = {
        new TableQuery[LiftedTable](tag => new LiftedTable(tag)) {
          override lazy val toNode = new SlickReifier(driver).tableExpansion(typeTag[T])
        }
      }
    }
  }
}
