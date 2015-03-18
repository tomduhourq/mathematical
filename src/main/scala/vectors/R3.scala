package vectors

import utils.Monoid
import vectors._

/**
 * Representation of the 3-dimensional space.
 */
object R3 {
  lazy val i = V3(1, 0, 0)
  lazy val j = V3(0, 1, 0)
  lazy val k = V3(0, 0, 1)

  def resolveX(u: V3, v: V3): V3 =
    i * (u.y * v.z - (v.y * u.z))

  def resolveY(u: V3, v: V3): V3 =
    (j * -1.0) * (u.x * v.z - (v.x * u.z))

  def resolveZ(u: V3, v: V3): V3 =
    k * (u.x * v.y - (v.x * u.y))

  // TODO: work a way to evaluate only monoids of Vectors
  def sum(l: List[Vector])(implicit m: Monoid[V3]) =
    Monoid.foldMap(l, m)(converter)

  lazy private val converter = (v: Vector) => v.toR3
}

object VMonoids {
  lazy val addMonoid = new Monoid[V3] {
    def operation(v1: V3, v2: V3) = v1 + v2
    def zero = V3.origin
  }
}


