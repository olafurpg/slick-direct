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
}

object Query extends SlickReflectUtil {

  def getTable[T: TypeTag]: Unit = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    println(table)
  }

  def tableExpansion(d: JdbcDriver, table: Table, tt: TypeTag[_]): TableExpansion = {
    val util = new SlickAstUtil {
      override val driver: JdbcDriver = d
    }
    val identitySymbol = SimpleTableIdentitySymbol(d, "_", table.name.table)
    val tableNode = util.tableNode(table.name.table)

    def col2node(c: Column): Node = {
      slick.ast.Select(Ref(util.sq_symbol), FieldSymbol(c.name)(Nil, util.stringColumnTypes(c.tpe))).nodeTyped(util.stringColumnTypes(c.tpe))
    }

    val mapping = TypeMapping(
      ProductNode(table.columns.map(col2node).toSeq),
      util.mappingsForT(tt),
      classTagFor(tt)
    )
    TableExpansion(util.sq_symbol, tableNode, mapping)
  }

  @reifyAs(TableExpansion)
  def apply[T: TypeTag](implicit driver: JdbcDriver): Query[T] = new Query[T] {
    /**
     * The accumulated AST in this query
     */
    override protected def ast: Node = {
      val tt = typeTag[T]
      val table = getTableFromSymbol(tt.tpe.typeSymbol)
      tableExpansion(driver, table, tt)
    }
  }
}
