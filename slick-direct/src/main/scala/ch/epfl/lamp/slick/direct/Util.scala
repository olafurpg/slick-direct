package ch.epfl.lamp.slick.direct

import slick.driver.JdbcDriver
import scala.reflect.runtime.universe._
import slick.{ ast => sq }
import slick.ast.AnonSymbol
import scala.reflect.ClassTag
import scala.reflect.runtime.{ currentMirror => cm, universe => ru}

trait DirectSlickModule[T] {
  val driver: JdbcDriver
  val typetag: TypeTag[T]
}

trait SlickReflectUtil[T] extends DirectSlickModule[T] {

  val classTagForT = ClassTag(typetag.mirror.runtimeClass(typetag.tpe))

  def tableName: String = typetag.tpe.typeSymbol.name.toString

  def constructorArgs(v: Any) =
    v match {
      case v: Vector[_] => v
      case v: Product => v.productIterator.toVector
    }

  def any2instanceOfT(v: Any) = {
    val constructor = cm.reflectClass(cm.classSymbol(cm.runtimeClass(typetag.tpe)))
      .reflectConstructor(
        typetag.tpe.member(ru.termNames.CONSTRUCTOR).asMethod
      )
    constructor.apply(constructorArgs(v): _*)
  }

  val members = {
    typetag.tpe.member(ru.termNames.CONSTRUCTOR).asMethod.paramLists.head
  }

}

trait SlickAstUtil[T] extends DirectSlickModule[T] with SlickReflectUtil[T] {

  val tableRef = new AnonSymbol

  val mappingsForT = sq.MappedScalaType.Mapper(_ => ???, any2instanceOfT, None)

  val columnTypes = {
    import driver.columnTypes._
    Map( // FIXME use symbols instead of strings for type names here
      typeOf[Int].typeSymbol     -> intJdbcType
      ,typeOf[Double].typeSymbol  -> doubleJdbcType
      ,typeOf[String].typeSymbol  -> stringJdbcType
      ,typeOf[Boolean].typeSymbol -> booleanJdbcType
    )
  }

  def columnField(name: String, typ: driver.DriverJdbcType[_]): sq.FieldSymbol = sq.FieldSymbol(name)(Nil, typ)

  def columnSelect(name: String, typ: driver.DriverJdbcType[_], sqSymbol: sq.Node): sq.Select =
    sq.Select(sqSymbol, columnField(name, typ)).nodeTyped(typ)

  def tableIdentity(name: String) = sq.SimpleTableIdentitySymbol(driver, "_", name)

  def tableNode(name: String) = {
    val tsym = tableIdentity(name)
    sq.TableNode(None, name, tsym, null, tsym)
  }

  // TODO: custom mappings, e.g., mappedTo
  def literalNode[T:WeakTypeTag](e: T): sq.LiteralNode = {
    val col = columnTypes(weakTypeOf[T].typeSymbol)
    sq.LiteralNode(col, e)
  }

  def selectMember(member: Symbol) =
    columnSelect(member.name.toString.trim, columnTypes(member.typeSignature.typeSymbol), sq.Ref(tableRef))
}

class SlickQueryMapper[T](val driver: JdbcDriver, val typetag: TypeTag[T])
  extends DirectSlickModule[T]
  with SlickAstUtil[T]
  with SlickReflectUtil[T] {
  def toNode[T](q: SlickQuery[T]): sq.Node =
    q match {
      case Const(e) => literalNode(e)
      // TODO: Do I need a new typetag for recursive calls?
      case Take(lhs, n) => sq.Take(toNode(lhs), sq.LiteralNode[Long](n.e.toLong))
      case Select_*() => {
        val mapping = sq.TypeMapping(
          sq.ProductNode(members.map(selectMember).toSeq),
          mappingsForT,
          classTagForT
        )
        sq.TableExpansion(tableRef, tableNode(tableName), mapping)
      }
      case _ => ???
    }
}
