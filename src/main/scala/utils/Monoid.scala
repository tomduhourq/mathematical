package utils

/**
 * Semigroup abstraction: An entity holding a
 * single operation for an invariant type.
 */
trait Semigroup[M] {
  def operation(v1: M, v2: M): M
}

/**
 * Monoid abstraction: An entity holding a zero
 * and a single operation for an invariant type.
 */
trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  def concatenate[A](as: List[A], m: Monoid[A])           =
    as.foldLeft(m.zero)(m.operation)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) =
    concatenate(as map f, m)
}
