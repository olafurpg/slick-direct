package ch.epfl.lamp.slick

import ch.epfl.directembedding.{ DETransformer, DslConfig }
import slick.ast.LiteralNode
import slick.dbio.NoStream

import scala.reflect.macros.{ Universe, Context, blackbox }

import slick.driver.H2Driver.api._
import scala.reflect.runtime.universe.TypeTag

package object direct {

  def DIRECT: Nothing = ???
  def query[T](block: T): T = macro implementations.lift[T]
  def queryDebug[T](block: T): T = macro implementations.liftDebug[T]

  implicit def lit2const[T](e: T): Const[T] = Const(e)

  implicit def createQueryActionExtensionMethodsFromSlickQuery[T: TypeTag](q: SlickQuery[T]) = {
    slickDriver.createQueryActionExtensionMethods[Seq[T], NoStream](slickDriver.queryCompiler.run(q.toNode).tree, ())
  }

  implicit def createQueryActionExtensionMethodsFromDirectQuery[T: TypeTag](q: Query[T]) = {
    slickDriver.createQueryActionExtensionMethods[Seq[T], NoStream](slickDriver.queryCompiler.run(q.toNode).tree, ())
  }

  object Config extends Config

  trait Config extends DslConfig
    with VirtualizationOverrides {
    // We transform them manually in PostProcessing
    override val virtualizeFunctions: Boolean = false

    // TODO: Get rid of these and manually typecheck that members exist?
    type Literal[T] = slick.ast.Node
    type Rep[T] = slick.ast.Node

    // TODO: Do we want the result to be from slick.ast?
    def compile[T](e: slick.ast.Node): Query[T] =
      new Query[T] {
        def ast = e
        type Self = T
        def shaped = ???
      }

    def dsl[T](e: Rep[T]): T = ???

    def lift[T](e: T): Literal[T] = e match {
      case n: Int => LiteralNode(n)
      case q: Query[_] => q.toNode
      case _ => ???
    }
  }

  trait VirtualizationOverrides {

  }

  object DslConfig extends Config

  object implementations {
    def lift[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(false)(c)(block)

    def liftDebug[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(true)(c)(block)

    def liftRep[T](debug: Boolean)(c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] = {
      val postProcessing = new ProjectionProcessing[c.type](c)
      DETransformer[c.type, T, Config](c)(
        "slick-direct",
        DslConfig,
        Map.empty,
        None,
        Some(postProcessing),
        debug
      ).apply(block)
    }
  }
}

