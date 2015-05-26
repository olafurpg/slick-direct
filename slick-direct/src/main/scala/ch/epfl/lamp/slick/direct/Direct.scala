package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.{ reifyAsInvoked, reifyAs }
import ch.epfl.lamp.slick.direct
import slick.profile.RelationalProfile
import slick.{ lifted, ast }
import slick.ast._
import slick.driver.{ H2Driver, JdbcDriver }
import slick.lifted._
import slick.model.{ Table, Column, QualifiedName }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }

trait Query[T, C[_]] {
  /**
   * The accumulated AST in this query
   */
  def lift: slick.lifted.Query[Table, T, C]

  type Table = AbstractTable[T]

  type Self = T
  // TODO: add shape to query

  @reifyAsInvoked
  def take(i: Long): Query[T, C] = ???

  @reifyAsInvoked
  def map[U](f: T => U): Query[U, C] = ???

  @reifyAsInvoked
  def flatMap[U, D[_]](f: T => Query[U, D]): Query[U, D] = ???

  @reifyAsInvoked
  def filter(f: T => Boolean): Query[T, C] = ???

  @reifyAsInvoked
  def filterNot(f: T => Boolean): Query[T, C] = ???

  @reifyAsInvoked
  def withFilter(f: T => Boolean): Query[T, C] = ???

  @reifyAsInvoked
  def join[T2, D[_]](q: Query[T2, D]): BaseJoinQuery[T, T2, T, T2, C] = ???

  @reifyAsInvoked
  def joinLeft[T2, D[_]](q: Query[T2, D]): BaseJoinQuery[T, Option[T2], T, T2, C] = ???

  @reifyAsInvoked
  def joinRight[T2, D[_]](q: Query[T2, D]): BaseJoinQuery[Option[T], T2, T, T2, C] = ???

  @reifyAsInvoked
  def joinFull[T2, D[_]](q: Query[T2, D]): BaseJoinQuery[Option[T], Option[T2], T, T2, C] = ???

}

trait BaseJoinQuery[T1, T2, J1, J2, C[_]] extends Query[(T1, T2), C] {

  @reifyAsInvoked
  def on(cond: (J1, J2) => Boolean): Query[(T1, T2), C] = ???

}

object SlickReification extends SlickReflectUtil {
  import slick.driver.H2Driver.api._

  def bootstrap[T](tableQuery: TableQuery[AbstractTable[T]]): BootstrappedTable[T] = BootstrappedTable(tableQuery)

  // TODO: Make generic
  def slick_int_===[T](lhs: lifted.Rep[Int], rhs: lifted.Rep[Int]): Rep[Option[Boolean]] = {
    columnExtensionMethods(lhs) === rhs
  }

  def slick_string_===[T](lhs: lifted.Rep[String], rhs: lifted.Rep[String]): Rep[Option[Boolean]] = {
    columnExtensionMethods(lhs) === rhs
  }

  def string_++(lhs: lifted.Rep[String], rhs: lifted.Rep[String]): Rep[String] = {
    lhs ++ rhs
  }

  // We explicitly provide T during projection processing
  def column[T, C](e: AnyRef, field: Rep[String], typ: Rep[String]): Rep[C] = {
    val f = field.asInstanceOf[LiteralColumn[String]]
    // TODO: Move this into ProjectionProcessing, and use
    // implicit conversion from any type C to ScalaBaseType
    val tt = typ.asInstanceOf[LiteralColumn[String]].value match {
      case "scala.Int" =>
        new ScalaBaseType[Int]
      case "java.lang.String" =>
        new ScalaBaseType[String]
    }

    val result = e.asInstanceOf[H2Driver.Table[T]].column[C](f.value)(tt.asInstanceOf[TypedType[C]])
    result
  }

  def select_*[T](driver: JdbcDriver, tt: TypeTag[T]): lifted.Query[AbstractTable[T], T, Seq] = {
    val table = getTable[T](tt)
    class LiftedTable(tag: Tag) extends driver.Table[T](tag, table.name.table) {
      def * = ??? // We override toNode
    }

    val q: TableQuery[LiftedTable] = new TableQuery[LiftedTable](tag => new LiftedTable(tag)) {
      override lazy val toNode = new SlickReifier(driver).tableExpansion(tt)
    }

    q
  }

  private def getTable[T: TypeTag]: slick.model.Table = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    table
  }
}

/**
 * Starting point for a direct query, equal to SELECT *
 *
 * Analogous to [[slick.lifted.TableQuery]]
 * @tparam T Type to be queried
 */
trait BaseQuery[T] extends Query[T, Seq] {
  // Is provided during embedding
  def tableQuery: TableQuery[Table] = ???
  def lift = tableQuery.asInstanceOf[lifted.Query[Table, T, Seq]]

}

object Query extends SlickReflectUtil {
  @reifyAs(SlickReification.select_* _)
  def apply[T]: BaseQuery[T] = new BaseQuery[T] {}

}
