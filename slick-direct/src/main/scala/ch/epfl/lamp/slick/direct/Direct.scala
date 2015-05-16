package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs

import scala.reflect.runtime.universe.TypeTag

trait Query[T] {
  def flatMap[S](projection: T => Query[S]): Query[S] = ???
  def withFilter(projection: T => Boolean): Query[T] = ???
  def length: Int = ???

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???

  @reifyAs(MapAction)
  def map[U](f: T => U): Query[U] = ???

}

@reifyAs(Select_*)
case class BaseQuery[T](tt: TypeTag[T]) extends Query[T]

object Query {
  def apply[T](implicit tt: TypeTag[T]): Query[T] = BaseQuery(tt)
}
