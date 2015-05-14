package ch.epfl.lamp.slick.direct

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }

trait SlickReflectUtil {

  def classTagFor(tt: TypeTag[_]) = ClassTag(tt.mirror.runtimeClass(tt.tpe))

  def tableName(tt: TypeTag[_]): String = tt.tpe.typeSymbol.name.toString

  def constructorArgs(v: Any) =
    v match {
      case v: Vector[_] => v
      case v: Product => v.productIterator.toVector
    }

  def any2instanceOfT(tt: TypeTag[_])(v: Any) = {
    val constructor = cm.reflectClass(cm.classSymbol(cm.runtimeClass(tt.tpe)))
      .reflectConstructor(
        tt.tpe.member(ru.termNames.CONSTRUCTOR).asMethod
      )
    constructor.apply(constructorArgs(v): _*)
  }

  def members(tt: TypeTag[_]): List[Symbol] = {
    tt.tpe.widen.member(ru.termNames.CONSTRUCTOR).asMethod.paramLists.head
  }

  def isNullable( sym:Symbol ) = sym == typeOf[Option[_]].typeSymbol
  def isNullable( tpe:Type ) : Boolean = isNullable(tpe.typeSymbol)

  def underlyingTypeSymbol( tpe:Type ) : Symbol =
    if( isNullable(tpe) )
      tpe match {
        case TypeRef(_,_,args) => args(0).typeSymbol
        case t => throw new Exception("failed to compute underlying type of "+tpe)
      }
    else tpe.typeSymbol

  // TODO: Some intelligent mapper from T
  // With annotations, etc.
  def sym2String( sym:Symbol ): String = sym.name.decodedName.toString.trim

  def columnName( sym:Symbol ): String = sym.name.decodedName.toString.trim
}

case class ReflectUtil[T](val typetag: TypeTag[T]) extends SlickReflectUtil

