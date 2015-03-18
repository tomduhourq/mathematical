package utils

/**
 * Monoid abstraction: An entity holding a zero
 * and a single operation for an invariant type.
 */
trait Monoid[A] {
  def operation(v1: A, v2: A): A
  def zero: A
}

object Monoid {
  def concatenate[A](as: List[A], m: Monoid[A]) =
    as.foldLeft(m.zero)(m.operation)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) =
    concatenate(as map f, m)
}
