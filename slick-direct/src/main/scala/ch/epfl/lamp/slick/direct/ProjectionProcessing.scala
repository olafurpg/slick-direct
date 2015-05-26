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
  extends PreProcessing(ctx)(Nil) {

  import c.universe._

  override val PreProcess = new (Tree => Tree) {
    def apply(tree: Tree) = {
      val withTables = new Direct2LiftedPreprocessing().transform(tree)
      withTables
    }
  }

  /**
   * Performs any necessary transformation from a slick.direct query to slick.lifted query
   *
   * Transformations fall into two categories:
   *
   *   - direct.Query[T] => TableQuery[driver.Table[T]]
   *   - (u: T).field => driver.column[T]("field")
   *
   * @param ctx Map from TermNames, that correspond to Queries in closures, to their types.
   */
  // TODO ctx: Map[TermName, Tree]
  private final class Direct2LiftedPreprocessing(ctx: Map[String, Tree] = Map.empty) extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case Function(lhs, rhs) =>
          val newCtx = tree.collect {
            case ValDef(_, TermName(name), tpt, _) =>
              name -> TypeTree(tpt.tpe.widen.dealias)
          }.toMap
          val args = lhs.map { vd =>
            // TODO: Can typeTransformer help here?
            ValDef(vd.mods, vd.name, TypeTree(c.typeOf[AnyRef]), vd.rhs)
          }
          Function(args, new Direct2LiftedPreprocessing(newCtx).transform(rhs))

        case s @ Select(lhs @ Ident(TermName(obj)), TermName(field)) if ctx.contains(obj) =>
          // TODO: Make configurable
          q"liftColumnSelect[${ctx(obj)}, ${s.tpe.widen.dealias}]($lhs, ${Literal(Constant(field))}, ${Literal(Constant(s.tpe.widen.typeSymbol.fullName))})"
        case t if tree.tpe <:< c.typeOf[direct.BaseQuery[_]] =>
          println(showRaw(t))
          val typ = tree.tpe.widen.dealias
          val innerTyp = typ.typeArgs.head
          q"""
          {
            ${table(innerTyp)}
            bootstrap[$innerTyp](TableQuery.apply(tag => new LiftedTable(tag)))
          }
          """
        case _ => {
          super.transform(tree)
        }

      }
    }
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
      val params = constructorParams(typ)
      q"""
          class LiftedTable(tag: Tag)
                  extends Table[$typ](tag, ${typ.typeSymbol.name.toString}) {
            ..${members(typ, params)}
            ${shape(typ, params)}
          }
        """
    }
  }

}

