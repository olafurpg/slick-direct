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

  lazy val columnTypes: Map[Symbol, driver.DriverJdbcType[_]] = {
    import driver.columnTypes._
    Map( // FIXME use symbols instead of strings for type names here
      typeOf[Int].typeSymbol -> intJdbcType, typeOf[Double].typeSymbol -> doubleJdbcType, typeOf[String].typeSymbol -> stringJdbcType, typeOf[Boolean].typeSymbol -> booleanJdbcType
    )
  }

  lazy val stringColumnTypes: Map[String, driver.DriverJdbcType[_]] = {
    import driver.columnTypes._
    Map(
      "Int" -> intJdbcType,
      "String" -> stringJdbcType
    )
  }

  def columnType(tpe: Type) = {
    val underlying = columnTypes(underlyingTypeSymbol(tpe))
    if (isNullable(tpe)) {
      underlying.optionType
    }
    else {
      underlying
    }
  }

  def columnField(name: String, typ: driver.DriverJdbcType[_]): sq.FieldSymbol = sq.FieldSymbol(name)(Nil, typ)

  private def columnField(sym: Symbol) =
    sq.FieldSymbol(columnName(sym))(
      if (isNullable(sym)) List(SqlProfile.ColumnOption.Nullable)
      else List(), columnType(sym.typeSignature)
    )

  def tableIdentity(name: String) = sq.SimpleTableIdentitySymbol(driver, "_", name)

  def tableNode(name: String) = {
    val tsym = tableIdentity(name)
    sq.TableNode(None, name, tsym, null, tsym)
  }

  // TODO: custom mappings, e.g., mappedTo
  def literalNode[T: WeakTypeTag](e: T): sq.LiteralNode = {
    val col = columnTypes(weakTypeOf[T].typeSymbol)
    sq.LiteralNode(col, e)
  }

  def selectMember(member: Symbol) =
    columnSelect(member, sq.Ref(sq_symbol))

  def columnSelect(sym: Symbol, sq_symbol: sq.Node) =
    sq.Select(
      sq_symbol,
      columnField(sym)
    ).nodeTyped(columnType(sym.typeSignature))


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