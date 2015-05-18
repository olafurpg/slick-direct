package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
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
  type Self
  protected def shaped: ShapedValue[T, _]
  // TODO: add shape to query

  def toNode = ast

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???

  @reifyAs(MapQuery)
  def map[U, V, W](f: T => U)(implicit shape: Shape[_ <: FlatShapeLevel, U, V, W]): Query[U] = ???

  @reifyAs(FlatMapQuery)
  def flatMap[U](f: T => Query[U]): Query[U] = ???
}

object MapQuery {
  // WIP
  def apply[U, V, W](self: Query[_])(f: self.Self => U): Any = {
    val q: Query[U] = new Query[U] {
      def shaped = ???
      type Self = U
      def ast = self.toNode
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
    type Self = T
    def shaped = ???
  }
}
