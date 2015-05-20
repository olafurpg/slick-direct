package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
import ch.epfl.lamp.slick.direct
import slick.{lifted, ast}
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
  def lift: slick.lifted.Rep[T]
  type Self = T
  // TODO: add shape to query

  @reifyAs(SlickReification.take _)
  def take(i: Int): Query[T] = ???

  def map(f: T => String): Query[String] = ???

  def flatMap[U](f: T => Query[U]): Query[U] = ???
}

object SlickReification extends SlickReflectUtil {
  def take[T, C[_]](self: lifted.Rep[direct.Query[C[T]]] , i: lifted.Rep[Long]): lifted.Rep[C[T]] = {
    // This casting is necessary, because we loose so much information by lifting everything to Rep[T]
    self.asInstanceOf[lifted.Query[AbstractTable[T], T, C]].take(i.asInstanceOf[ConstColumn[Long]])
  }

  // This does not compile because lift(e: T): Rep[T]
//  def take[T, C[_]](self: lifted.Query[AbstractTable[T], T, C], i: ConstColumn[Long]): lifted.Rep[C[T]] = {
//    // This casting is necessary, because we loose so much information by lifting everything to Rep[T]
//    self.take(i)
//  }

}

object Query extends SlickReflectUtil {

  def getTable[T: TypeTag]: slick.model.Table = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
//    println(table)
    table
  }

  @reifyAs(TableExpansion)
  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[Seq[T]] = {
    val table = getTable[T]
    new Query[Seq[T]] {

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
