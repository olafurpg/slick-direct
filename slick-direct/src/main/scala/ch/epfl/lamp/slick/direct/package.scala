package ch.epfl.lamp.slick

import ch.epfl.directembedding.transformers.{ reifyAsInvoked, reifyAs }
import ch.epfl.directembedding.{ DETransformer, DslConfig }
import slick.ast.{ TypedType, LiteralNode }
import slick.dbio.NoStream
import slick.lifted
import slick.lifted.AbstractTable

import scala.reflect.macros.{ Universe, Context, blackbox }

import slick.driver.H2Driver.api._
import scala.reflect.runtime.universe.TypeTag

package object direct {

  def INTERNAL: Nothing = ???

  def INTERNAL[T](msg: => T): Nothing = {
    println(msg)
    throw new NotImplementedError(s"$msg")
  }

  def query[T](block: T): T = macro implementations.lift[T]
  def queryDebug[T](block: T): T = macro implementations.liftDebug[T]

  implicit def createQueryActionExtensionMethodsFromDirectQuery[T: TypeTag, C[_]](q: direct.Query[T, C]) = {
    slickDriver.createQueryActionExtensionMethods[Seq[T], NoStream](slickDriver.queryCompiler.run(q.lift.toNode).tree, ())
  }

  implicit def query2rep[T, C[_]](q: direct.Query[T, C]): lifted.Query[q.Table, T, C] = q.lift

  trait FakeSlickRep[T] extends slick.lifted.Rep[T] {
    def encodeRef(path: slick.ast.Node): slick.lifted.Rep[T] = ???
    def toNode: slick.ast.Node = ???
  }

  case class SlickColField[T](name: String) extends FakeSlickRep[T]

  object Config extends Config

  trait Config extends DslConfig
    with VirtualizationOverrides {
    // We transform them manually in PostProcessing
    override val virtualizeFunctions: Boolean = false
    override val embedFunctions: Boolean = true
    override val flattenCurriedFunctions: Boolean = false

    // TODO: Get rid of these and manually typecheck that members exist?
    type Literal[T] = slick.lifted.Rep[T]

    type Rep[T] = slick.lifted.Rep[T]

    // TODO: Do we want the result to be from slick.ast?
    def compile[T, C[_]](e: Rep[C[T]]): direct.Query[T, C] =
      new Query[T, C] {
        // This cast must succeed
        def lift = e.asInstanceOf[lifted.Query[AbstractTable[T], T, C]]
      }

    // Method overloading
    def compile[T, T2, C[_]](e: Rep[C[(T, T2)]]): direct.BaseJoinQuery[T, T2, C] =
      new BaseJoinQuery[T, T2, C] {
        def lift = e.asInstanceOf[lifted.Query[AbstractTable[(T, T2)], (T, T2), C]]
      }

    def dsl[T](e: Rep[T]): T = ???

    def constColumnLift[T](e: T): ConstColumn[T] = e match {
      case n: Int => new LiteralColumn(n).asInstanceOf[ConstColumn[T]]
      case n: Long => new LiteralColumn(n).asInstanceOf[ConstColumn[T]]
      case _ => INTERNAL(e)
    }

    def queryLift[T, C[_]](e: direct.Query[T, C]): lifted.Query[e.Table, T, C] = e.lift

    def lift[T](e: T): Rep[T] = e match {
      case _: Rep[T] => e.asInstanceOf[Rep[T]]
      case s: String => new LiteralColumn(s).asInstanceOf[Rep[T]]
      case b: Boolean => new LiteralColumn(b).asInstanceOf[Rep[T]]
      case _ => INTERNAL(e)
    }
  }

  trait VirtualizationOverrides {

    @reifyAsInvoked
    def <[T](a: T, b: T): Boolean = ???

    @reifyAs(SlickReification.column _)
    def liftColumnSelect[T, C](e: T, fieldName: String, typ: String): C = ???

    @reifyAs(SlickReification.slick_int_=== _)
    def infix_==(a: Int, b: Int): Boolean = ???

    @reifyAs(SlickReification.slick_string_=== _)
    def infix_==(a: String, b: String): Boolean = ???

  }

  class MyInt {
    @reifyAsInvoked
    def <(that: Int): Boolean = ???

    @reifyAsInvoked
    def >(that: Int): Boolean = ???
  }

  class MyString {
    @reifyAs(SlickReification.string_++ _)
    def +(that: String): String = ???
  }

  class MyTuple {
    @reifyAsInvoked
    def apply(a: AnyRef*) = ???
  }

  object DslConfig extends Config

  object implementations {
    def lift[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(false)(c)(block)

    def liftDebug[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(true)(c)(block)

    def liftRep[T](debug: Boolean)(c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] = {
      val preProcessing = new ProjectionProcessing[c.type](c)
      println("DETransformation...")
      DETransformer[c.type, T, Config](c)(
        "slick-direct",
        DslConfig,
        Map(
          c.typeOf[Int] -> c.typeOf[MyInt],
          c.typeOf[String] -> c.typeOf[MyString]
        ),
        Map(
          // TODO: Can we avoid hardcoding Seq here?
          c.typeOf[Int] -> "constColumnLift",
          c.typeOf[Long] -> "constColumnLift",
          c.typeOf[direct.Query[_, Seq]] -> "queryLift"
        ),
        //        Set(c.typeOf[Query[_]]),
        Set.empty,
        Some(preProcessing),
        None,
        if (debug) 4 else 0
      ).apply(block)
    }
  }
}

