package ch.epfl.lamp.slick.direct

import slick.driver.JdbcDriver
import slick.model.{Column, Table}
import slick.profile.SqlProfile
import scala.reflect.runtime.universe._
import slick.{ ast => sq }

/**
 * Utility methods for dealing with the slick.ast API
 *
 * Many of these methods are blatantly copied from SlickBackend in the Slick repo
 */
trait SlickAstUtil extends DirectSlickModule with SlickReflectUtil {

  lazy val sq_symbol = new sq.AnonSymbol

  def mappingsForT(tt: TypeTag[_]) = sq.MappedScalaType.Mapper(_ => ???, any2instanceOfT(tt), None)

  lazy val stringColumnTypes: Map[String, driver.DriverJdbcType[_]] = {
    import driver.columnTypes._
    Map(
      "Int" -> intJdbcType,
      "String" -> stringJdbcType
    )
  }

  def tableIdentity(name: String) = sq.SimpleTableIdentitySymbol(driver, "_", name)

  def tableNode(name: String) = {
    val tsym = tableIdentity(name)
    sq.TableNode(None, name, tsym, null, tsym)
  }

  def tableExpansion(tt: TypeTag[_]): sq.TableExpansion = {
    val table = getTableFromSymbol(tt.tpe.typeSymbol)

    def col2node(c: Column): sq.Node = {
      sq.Select(sq.Ref(sq_symbol), sq.FieldSymbol(c.name)(Nil, stringColumnTypes(c.tpe))).nodeTyped(stringColumnTypes(c.tpe))
    }

    val mapping = sq.TypeMapping(
      sq.ProductNode(table.columns.map(col2node)),
      mappingsForT(tt),
      classTagFor(tt)
    )
    sq.TableExpansion(sq_symbol, tableNode(table.name.table), mapping)
  }
}

class SlickReifier(val driver: JdbcDriver) extends SlickAstUtil