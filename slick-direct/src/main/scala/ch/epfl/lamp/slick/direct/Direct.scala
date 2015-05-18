package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
import slick.ast._
import slick.driver.JdbcDriver
import slick.lifted.TableQuery
import slick.model.{ Table, Column, QualifiedName }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }

trait Query[T] {
  /**
   * The accumulated AST in this query
   */
  protected def ast: slick.ast.Node

  def toNode = ast

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???
}

object Query extends SlickReflectUtil {

  def getTable[T: TypeTag]: Unit = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    println(table)
  }


  @reifyAs(TableExpansion)
  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[T] = new Query[T] {
    override protected def ast: Node = {
      new SlickReifier(driver).tableExpansion(typeTag[T])
    }
  }
}
