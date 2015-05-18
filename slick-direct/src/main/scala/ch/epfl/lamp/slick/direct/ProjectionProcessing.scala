package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.DirectEmbeddingUtils
import ch.epfl.yinyang.transformers.{ PostProcessing, PreProcessing }

import scala.reflect.macros.blackbox.Context

class ProjectionProcessing[C <: Context](ctx: C) extends PostProcessing(ctx)(Nil) {
  import c.universe._

  override val PostProcess = new (Tree => Tree) {
    def apply(tree: Tree) = new FieldExtractor().transform(tree)
  }

  private final class FieldExtractor extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      // TODO: Extract all selected fields
      //      case Function(args, s @ Select(_, TermName(field))) =>
      //        Literal(Constant(field))
      case Function(lhs, rhs) =>
        val fields = rhs.collect {
          case Select(_, TermName(field)) => Literal(Constant(field))
        }
        q"List(..${fields})"
        fields.last
      //        println(showRaw(tree))
      //        transform(Function(lhs, transform(rhs)))

      case _ => super.transform(tree)
    }
  }
}

