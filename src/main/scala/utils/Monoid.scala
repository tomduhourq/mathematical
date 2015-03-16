package utils

/**
 * Monoid abstraction: An entity holding a zero
 * and a single operation for an invariant type.
 */
trait Monoid[A] {
  def operation(v1: A, v2: A): A
  def zero: A
}
