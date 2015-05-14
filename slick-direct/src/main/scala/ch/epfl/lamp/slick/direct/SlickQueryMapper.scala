package ch.epfl.lamp.slick.direct
import slick.driver.JdbcDriver
import scala.reflect.runtime.universe._
import slick.{ ast => sq }

trait DirectSlickModule {
  val driver: JdbcDriver
}

class SlickQueryMapper(val driver: JdbcDriver)
  extends DirectSlickModule
  with SlickAstUtil
  with SlickReflectUtil {

  def toNode[T](q: SlickQuery[T]): sq.Node = {
    q match {
      case Const(e) => e match {
          // We hit a leaf node
        case s @ BaseQuery(tt) => Select_*(tt).toNode(driver)
        case _ => literalNode(e)
      }
      case Take(lhs, n) => sq.Take(toNode(lhs), sq.LiteralNode[Long](n.e.toLong))
      case Select_*(tt) => {
        val mapping = sq.TypeMapping(
          sq.ProductNode(members(tt).map(selectMember).toSeq),
          mappingsForT(tt),
          classTagFor(tt)
        )
        sq.TableExpansion(sq_symbol, tableNode(tableName(tt)), mapping)
      }
      case MapAction(lhs, field) => {
        val sq_lhs = toNode(lhs)
        // TODO: What if it's missing?
        val tt = lhs.getTypeTag.get
        val symbol = members(tt).find(sym2String(_) == field).head
        val sq_rhs = columnSelect(symbol, sq.Ref(sq_symbol))
        sq.Bind(sq_symbol, sq_lhs, sq.Pure(sq_rhs))
      }
      case _ => ???
    }
  }
}
