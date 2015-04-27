package ch.epfl.lamp.slick.direct

import ch.epfl.directembedding.transformers.reifyAs

trait Query[T] {
  def flatMap[S](projection: T => Query[S]): Query[S] = ???
  def withFilter(projection: T => Boolean): Query[T] = ???
  def length: Int = ???

  @reifyAs(Take)
  def take(i: Int): Query[T] = ???

  @reifyAs(Drop)
  def drop(i: Int): Query[T] = ???

}

object Query {
  @reifyAs(Select_*)
  def apply[T]: Query[T] = ???
}
