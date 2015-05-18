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

  /**
   * Map class definition to slick.model.Table
   */
  private def getTableFromSymbol(classDefSymbol: Symbol): Table = {
    def getNameOfSymbol(symbol: Symbol): Option[String] = {
      // workaround for SI-7424
      symbol.typeSignature
      symbol.annotations.foreach(_.tree.tpe)
      symbol.annotations.headOption.map(x => x.tree.children.tail.head).flatMap(annotationToName)
    }
    def annotationToName(tree: Tree): Option[String] = tree match {
      case Literal(Constant(name: String)) => Some(name)
      case _ => None
    }
    val tName = classDefSymbol.name.toString()
    val tableName = getNameOfSymbol(classDefSymbol).getOrElse(tName)
    val tableQName = QualifiedName(tableName)
    val columns = classDefSymbol.typeSignature.member(ru.termNames.CONSTRUCTOR).typeSignature match {
      case MethodType(params, resultType) => params map { param =>
        val cName = param.name.toString()
        val columnName = getNameOfSymbol(param).getOrElse(cName)
        val tpe = param.typeSignature.dealias.typeSymbol.name.decodedName.toString
        // TODO: Options, nullable, etc
        Column(columnName, tableQName, tpe, false)
      }
    }
    // TODO: Indices and FK
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
