package ch.epfl.lamp.slick

import ch.epfl.directembedding.{ DETransformer, DslConfig }

import scala.reflect.macros.blackbox

package object direct {
  def DIRECT: Nothing = ???
  def query[T](block: Query[T]): SlickQuery[T] = macro implementations.lift[T]
  def queryDebug[T](block: Query[T]): SlickQuery[T] = macro implementations.liftDebug[T]

  implicit def lit2const[T](e: T): Const[T] = Const(e)

  object Config extends Config

  trait Config extends DslConfig
    with VirtualizationOverrides {

    // TODO: Get rid of these and manually typecheck that members exist?
    type Literal[T] = direct.Const[T]
    type Rep[T] = direct.SlickQuery[T]

    // TODO: Do we want the result to be from slick.ast?
    def compile[T](e: Rep[T]): Rep[T] = e

    def dsl[T](e: Rep[T]): T = ???

    def lift[T](e: T): Literal[T] = Const(e)
  }

  trait VirtualizationOverrides {

  }

  object implementations {
    def lift[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(false)(c)(block)

    def liftDebug[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(true)(c)(block)

    def liftRep[T](debug: Boolean)(c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] = {
      DETransformer[c.type, T, Config](c)(
        "slick-direct",
        Map.empty,
        None,
        // Note the explicit `apply`, this is necessary.
        None,
        debug
      ).apply(block)
    }
  }
}
