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
  protected def ast: slick.ast.Node
  def lift: slick.lifted.Query[Rep[T], _, Seq]
  type Self
  // TODO: add shape to query

  def toNode = ast

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???

  @reifyAs(SlickReification.map _)
  def map(f: T => String): Query[String] = ???

  @reifyAs(FlatMapQuery)
  def flatMap[U](f: T => Query[U]): Query[U] = ???
}

object MapQuery {
  // WIP
  def apply[U, V, W](self: Query[_])(f: self.Self => U): Any = {
    val q: Query[U] = new Query[U] {
      type Self = U
      def ast = self.toNode
      def lift = ???
    }
    FlatMapQuery[self.Self, U](self, v => q)
  }
}

object FlatMapQuery {
  // WIP
  def apply[T, U](self: Query[_], f: T => Query[U]): Any = {
    val generator = new AnonSymbol

    ???
  }
}

object SlickReification extends SlickReflectUtil {
  def map(lhs: Query[_], f: Function1[_, _]): Node = {
    val q = new Query[String] {
      def ast = lhs.toNode
      def lift = ???
      type Self = String
    }
    new slick.ast.Bind(new AnonSymbol, lhs.toNode, q.toNode)
  }

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
      override protected def ast: Node = {
        new SlickReifier(driver).tableExpansion(typeTag[T])
      }
      class LiftedTable(tag: Tag) extends driver.Table[T](tag, table.name.table) {
        def * = ??? // We override toNode
      }

      def lift = {
        new TableQuery[LiftedTable](tag => new LiftedTable(tag)) {
          override lazy val toNode = ast
        }
      }
      type Self = T
    }
  }
}
