package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.{preserveInvocation, reifyAs}
import ch.epfl.lamp.slick.direct
import slick.profile.RelationalProfile
import slick.{ lifted, ast }
import slick.ast._
import slick.driver.{H2Driver, JdbcDriver}
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

  @preserveInvocation
  def take(i: Long): Query[T, C] = ???

  @preserveInvocation
  def map[U](f: T => U): Query[U, C] = ???

  @preserveInvocation
  def flatMap[U, D[_]](f: T => Query[U, D]): Query[U, D] = ???

}

object SlickReification {

  // We explicitly provide T during projection processing
  def column[T, C](e: AnyRef, field: Rep[String], typ: Rep[String]): Rep[C] =  {
    val f = field.asInstanceOf[SlickColField[C]]
    // TODO: Move this into ProjectionProcessing, fail at compile time
    val tt = typ.asInstanceOf[SlickColField[C]].name match {
      case "scala.Int" =>
        new ScalaBaseType[Int]
      case "java.lang.String" =>
        new ScalaBaseType[String]
    }
    e.asInstanceOf[H2Driver.Table[T]].column[C](f.name)(tt.asInstanceOf[TypedType[C]])
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
