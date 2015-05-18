package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs
import slick.model.{Table, Column, QualifiedName}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{ currentMirror => cm, universe => ru }
import slick.ast


trait Query[T] {
  protected def ast: SlickQuery[T] = EmptyQuery()
//  @reifyAs(Take)
//  def take(i: Int): Query[T] = ???

}

object Query {

  def getTableFromSymbol(symbol: Symbol): Table = {
    def getNameOfSymbol(symbol: Symbol): Option[String] = {
      // workaround for SI-7424
      symbol.typeSignature
      symbol.annotations.foreach(_.tpe)
      symbol.annotations.headOption.map(_.scalaArgs.head).flatMap(annotationToName)
    }
    def annotationToName(tree: Tree): Option[String] = tree match {
      case Literal(Constant(name: String)) => Some(name)
      case _ => None
    }
    val tName = symbol.name.toString()
    val tableName = getNameOfSymbol(symbol).getOrElse(tName)
    val tableQName = QualifiedName(tableName)
    val columns = symbol.typeSignature.member(ru.termNames.CONSTRUCTOR).typeSignature match {
      case MethodType(params, resultType) => params map { param =>
        val cName = param.name.toString()
        val columnName = getNameOfSymbol(param).getOrElse(cName)
        val tpe = param.typeSignature.dealias.typeSymbol.name.decodedName.toString
        Column(columnName, tableQName, tpe, false)
      }
    }
    Table(tableQName, columns, None, Nil, Nil)
  }
  def getTable[T:TypeTag]: Unit = {
    val tt = typeTag[T]
    val table = getTableFromSymbol(tt.tpe.typeSymbol)
    println(table)
  }

  @reifyAs(ast.TableExpansion)
  def apply[T]: Query[T] = ???
}
