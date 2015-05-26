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

}

object SlickReification {
  import slick.driver.H2Driver.api._

  def ===[T](lhs: lifted.Rep[_ <: T], rhs: lifted.Rep[_ <: T]): Rep[Option[Boolean]] = {
    ???
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

}

object Query extends SlickReflectUtil {

  private def getTable[T: TypeTag]: slick.model.Table = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    table
  }

  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[T, Seq] = {
    val table = getTable[T]
    class LiftedTable(tag: Tag) extends driver.Table[T](tag, table.name.table) {
      def * = ??? // We override toNode
    }

    val q: TableQuery[LiftedTable] = new TableQuery[LiftedTable](tag => new LiftedTable(tag)) {
      override lazy val toNode = new SlickReifier(driver).tableExpansion(typeTag[T])
    }

    new Query[T, Seq] {

      def lift = q
    }
  }
}
