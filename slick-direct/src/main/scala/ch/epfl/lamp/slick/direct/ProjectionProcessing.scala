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
            case ValDef(_, TermName(name), _, _) => name
          }
          println(args)
          var fields = rhs.collect {
            case Select(Ident(TermName(obj)), TermName(field)) if args.contains(obj) =>
              Literal(Constant(field))
          }

          Function(lhs, q"List[String](..$fields)")
          Function(lhs, fields.head)
        case _ => super.transform(tree)
      }
    }
  }
}

