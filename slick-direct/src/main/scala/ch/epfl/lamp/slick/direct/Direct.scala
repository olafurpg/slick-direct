package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
import ch.epfl.lamp.slick.direct
import slick.{ lifted, ast }
import slick.ast._
import slick.driver.JdbcDriver
import slick.lifted._
import slick.model.{ Table, Column, QualifiedName }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }

trait Query[T, C[_]] {
  /**
   * The accumulated AST in this query
   */
  def lift: slick.lifted.Query[E, T, C]
  type E = AbstractTable[T]

  type Self = T
  // TODO: add shape to query

  @reifyAs(SlickReification.take _)
  def take(i: Long): Query[T, C] = ???

  @reifyAs(SlickReification.map _)
  def map[U](f: T => U): Query[U, C] = ???

}

object SlickReification extends SlickReflectUtil {
  def take[T, C[_], E](self: lifted.Query[E, T, C], i: lifted.ConstColumn[Long]): lifted.Query[E, T, C] = {
    self.take(i)
  }

  def map[E, U, C[_], F, G, T](self: lifted.Query[E, U, C], f: E => F): lifted.Query[G, T, C] = {
    self.map(f)(???)
  }
}

object Query extends SlickReflectUtil {

  private def getTable[T: TypeTag]: slick.model.Table = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    //    println(table)
    table
  }

  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[T, Seq] = {
    val table = getTable[T]
    new Query[T, Seq] {

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
