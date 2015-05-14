package ch.epfl.lamp.slick.direct

import slick.profile.SqlProfile
import scala.reflect.runtime.universe._
import slick.{ ast => sq }
import slick.ast.AnonSymbol

/**
 * Utility methods for dealing with the slick.ast API
 *
 * Many of these methods are blatantly copied from SlickBackend in the Slick repo
 */
trait SlickAstUtil extends DirectSlickModule with SlickReflectUtil {

  lazy val sq_symbol = new AnonSymbol

  def mappingsForT(tt: TypeTag[_]) = sq.MappedScalaType.Mapper(_ => ???, any2instanceOfT(tt), None)

  val columnTypes = {
    import driver.columnTypes._
    Map( // FIXME use symbols instead of strings for type names here
      typeOf[Int].typeSymbol -> intJdbcType, typeOf[Double].typeSymbol -> doubleJdbcType, typeOf[String].typeSymbol -> stringJdbcType, typeOf[Boolean].typeSymbol -> booleanJdbcType
    )
  }

  def columnType( tpe:Type ) = {
    val underlying = columnTypes(underlyingTypeSymbol(tpe))
    if( isNullable(tpe) ){
      underlying.optionType
    } else {
      underlying
    }
  }

  def columnField(name: String, typ: driver.DriverJdbcType[_]): sq.FieldSymbol = sq.FieldSymbol(name)(Nil, typ)

  private def columnField( sym:Symbol ) =
    sq.FieldSymbol( columnName(sym) )(
      if( isNullable(sym) ) List(SqlProfile.ColumnOption.Nullable)
      else List()
      , columnType(sym.typeSignature)
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

  def columnSelect( sym:Symbol, sq_symbol:sq.Node ) =
    sq.Select(
      sq_symbol,
      columnField(sym)
    ).nodeTyped( columnType(sym.typeSignature) )
}
