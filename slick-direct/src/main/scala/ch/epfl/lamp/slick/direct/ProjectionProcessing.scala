package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.DirectEmbeddingUtils
import ch.epfl.lamp.slick.direct
import ch.epfl.yinyang.analysis.FreeIdentAnalysis
import ch.epfl.yinyang.transformers.{ PostProcessing, PreProcessing }

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context

class ProjectionProcessing[C <: blackbox.Context](ctx: C)
  extends PreProcessing(ctx)(Nil)
  with FreeIdentAnalysis {
  type Ctx = C
  override val c = ctx
  override val debugLevel = 0

  import c.universe._

  override val PreProcess = new (Tree => Tree) {
    def apply(tree: Tree) = {
      println(showRaw(tree, printTypes = true))
      val freeVars = freeVariables(tree).map(_.symbol)
      println(freeVars)
      // TODO: Make pipeline
      val withCols = new FieldExtractor(freeVars).transform(tree)
      val withTables = new TableProvider(freeVars).transform(withCols)
      println(showRaw(withTables))
      withTables
    }
  }

  private final class FieldExtractor(captured: List[Symbol]) extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case Function(lhs, rhs) =>
          val args = tree.collect {
            case ValDef(_, TermName(name), tpt, _) =>
              name -> TypeTree(tpt.tpe.widen.dealias)
          }.toMap
          val result = new ColumnSelect(args).transform(tree)
          result
        case _ => super.transform(tree)
      }
    }
  }

  private final class TableProvider(captured: List[Symbol]) extends Transformer {
    def shape(typ: Type, syms: List[Symbol]): DefDef = {
      val typName = typ.typeSymbol.name.toString
      val names = syms.map(_.name)
      val typIdent = Ident(TermName(typName))
      // TODO: Support types with companion object
      q"""
        def * = _root_.slick.lifted.ProvenShape.proveShapeOf((..$names) <> ($typIdent.tupled, $typIdent.unapply))
       """
    }

    def column(typ: Type)(member: Symbol): DefDef = {
      val sym = typ.member(member.name)
      val fieldType = sym.typeSignature
      q"""
          def ${TermName(sym.name.toString)}: slick.lifted.Rep[${fieldType}] = column[$fieldType](${Literal(Constant(sym.name.toString))});
       """
    }

    def members(typ: Type, params: List[Symbol]): Tree = {
      q"""
          ..${params.map(column(typ))}
       """
    }

    def constructorParams(typ: Type) = {
      val lst = typ.decls.collect {
        case m: MethodSymbol if m.isConstructor => m.paramLists
      }
      val result = lst.flatten.flatten
      result.toList
    }

    def table(typ: Type): ClassDef = {
      //      ${members(typ)}
      val params = constructorParams(typ)
      q"""
          class LiftedTable(tag: Tag)
                  extends Table[$typ](tag, ${typ.typeSymbol.name.toString}) {
            ..${members(typ, params)}
            ${shape(typ, params)}
          }
        """
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        // Generate  TableQuery for BaseQuery
        case t if tree.tpe <:< c.typeOf[direct.BaseQuery[_]] =>
          val typ = tree.tpe.widen.dealias
          val innerTyp = typ.typeArgs.head
          q"""
              {
                class Users(tag: Tag)
                  extends Table[User](tag, "User") {

                  def id: Rep[Int] = column[Int]("id", O.PrimaryKey)
                  def name: Rep[String] = column[String]("name")

                  def * = slick.lifted.ProvenShape.proveShapeOf((id, name) <> (User.tupled, User.unapply))
                }

                bootstrap[User](TableQuery.apply(tag => new Users(tag)))
              }
           """
          q"""
          {
            ${table(innerTyp)}
            bootstrap[$innerTyp](TableQuery.apply(tag => new LiftedTable(tag)))
          }
          """
        case _ => super.transform(tree)
      }
    }
  }

  /**
   * Convert all field accesses to liftColumnSelect
   * @param ctx Map from argument to argument's type tree
   */
  private final class ColumnSelect(ctx: Map[String, Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case Function(lhs, rhs) =>
          val args = lhs.map { vd =>
            // TODO: Can typeTransformer help here?
            ValDef(vd.mods, vd.name, TypeTree(c.typeOf[AnyRef]), vd.rhs)
          }
          Function(args, transform(rhs))

        case s @ Select(lhs @ Ident(TermName(obj)), TermName(field)) if ctx.contains(obj) =>
          // TODO: Make configurable
          q"liftColumnSelect[${ctx(obj)}, ${s.tpe.widen.dealias}]($lhs, ${Literal(Constant(field))}, ${Literal(Constant(s.tpe.widen.typeSymbol.fullName))})"

        case _ => super.transform(tree)
      }
    }
  }
}

