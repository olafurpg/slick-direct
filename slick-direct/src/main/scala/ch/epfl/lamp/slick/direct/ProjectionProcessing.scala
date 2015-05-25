package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.DirectEmbeddingUtils
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
          println(showRaw(tree))
          val args = lhs.collect {
            case ValDef(_, TermName(name), _, _) => name -> Ident(TermName(name))
          }.toMap
          val result = new ColumnSelect(args).transform(tree)
          println(result)
          println(showRaw(result))

          tree
          result
        case _ => super.transform(tree)
      }
    }
  }

  private final class ColumnSelect(args: Map[String, Ident]) extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case s @ Select(lhs @ Ident(TermName(obj)), TermName(field)) if args.contains(obj) =>
          q"liftColumn[User, ${s.tpe}]($lhs, ${Literal(Constant(field))}, implicitly[slick.ast.TypedType[${s.tpe}]])"

        case _ => super.transform(tree)
      }
    }
  }
}

