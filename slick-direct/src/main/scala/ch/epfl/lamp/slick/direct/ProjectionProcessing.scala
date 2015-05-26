package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.DirectEmbeddingUtils
import ch.epfl.lamp.slick.direct
import ch.epfl.yinyang.transformers.{ PostProcessing, PreProcessing }

import scala.reflect.macros.blackbox.Context

class ProjectionProcessing[C <: Context](ctx: C) extends PreProcessing(ctx)(Nil) {
  import c.universe._

  override val PreProcess = new (Tree => Tree) {
    def apply(tree: Tree) = new FieldExtractor().transform(tree)
  }

  private final class FieldExtractor extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case Function(lhs, rhs) =>
          val args = tree.collect {
            case ValDef(_, TermName(name), tpt, _) =>
              name -> TypeTree(tpt.tpe.widen.dealias)
          }.toMap
          val result = new ColumnSelect(args).transform(tree)
          result
          // Generate  TableQuery for BaseQuery
        case t if tree.tpe <:< c.typeOf[direct.BaseQuery[_]] =>
          val typ = tree.tpe.widen.dealias
          q"ch.epfl.lamp.slick.direct.Query[$typ]"
          q"""

              class Users(tag: Tag)
                extends Table[User](tag, "User") {

                def id: Rep[Int] = column[Int]("id", O.PrimaryKey)
                def name: Rep[String] = column[String]("name")

                def * = slick.lifted.ProvenShape.proveShapeOf((id, name) <> (User.tupled, User.unapply))
              }

              bootstrap[User](TableQuery.apply(tag => new Users(tag)))
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

