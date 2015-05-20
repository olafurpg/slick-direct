package ch.epfl.lamp.slick

import ch.epfl.directembedding.{ DETransformer, DslConfig }
import slick.ast.LiteralNode
import slick.dbio.NoStream
import slick.lifted

import scala.reflect.macros.{ Universe, Context, blackbox }

import slick.driver.H2Driver.api._
import scala.reflect.runtime.universe.TypeTag

package object direct {

  def DIRECT_INTERNAL: Nothing = ???

  def DIRECT_INTERNAL[T](msg: => T): Nothing = {
    println(msg)
    throw new NotImplementedError(s"$msg")
  }

  def query[T](block: T): T = macro implementations.lift[T]
  def queryDebug[T](block: T): T = macro implementations.liftDebug[T]

  implicit def lit2const[T](e: T): Const[T] = Const(e)

  implicit def createQueryActionExtensionMethodsFromSlickQuery[T: TypeTag](q: SlickQuery[T]) = {
    slickDriver.createQueryActionExtensionMethods[Seq[T], NoStream](slickDriver.queryCompiler.run(q.toNode).tree, ())
  }

  implicit def createQueryActionExtensionMethodsFromDirectQuery[T: TypeTag](q: direct.Query[T]) = {
    slickDriver.createQueryActionExtensionMethods[Seq[T], NoStream](slickDriver.queryCompiler.run(q.toNode).tree, ())
  }

  object Config extends Config

  trait Config extends DslConfig
    with VirtualizationOverrides {
    // We transform them manually in PostProcessing
    override val virtualizeFunctions: Boolean = false
    override val embedFunctions: Boolean = true
    override val flattenCurriedFunctions: Boolean = false

    // TODO: Get rid of these and manually typecheck that members exist?
    type Literal[T] = slick.lifted.QueryBase[_]
    type Rep[T] = slick.lifted.QueryBase[_]

    // TODO: Do we want the result to be from slick.ast?
    def compile[T](e: Rep[T]): direct.Query[T] =
      new Query[T] {
        def lift = e
      }

    def dsl[T](e: Rep[T]): T = ???

    def lift[T](e: T): Rep[T] = e match {
      case q: direct.Query[_] => q.lift
      case n: Int => ???
      case _ => {
        DIRECT_INTERNAL(e)
        ???
      }
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
      val preProcessing = new ProjectionProcessing[c.type](c)
      DETransformer[c.type, T, Config](c)(
        "slick-direct",
        DslConfig,
        Map.empty,
        //        Set(c.typeOf[Query[_]]),
        Set.empty,
        Some(preProcessing),
        None,
        debug
      ).apply(block)
    }
  }
}

