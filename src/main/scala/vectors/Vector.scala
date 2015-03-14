package vectors

import utils.NumberUtils._

/**
 * This is the general definition of a vector. All other vectors extend from this,
 * and must define the common operators.
 */
sealed trait Vector {
  def *(n: Int): Vector
}
/**
 * Vectors in R2
 */
case class VectorR2(x: Int, y: Int) extends Vector {

  def +(that: VectorR2) =
    VectorR2(x + that.x, y + that.y)

  def -(that: VectorR2) =
    this + VectorR2(-that.x, -that.y)

  def *(that: Int)      =
    VectorR2(x * that, y * that)
  def *(that: VectorR2) =
    x * that.x + y * that.y

  def x(that: VectorR2) = {
    val u = this.toR3
    val v: VectorR3 = that.toR3
    R3.resolveX(u, v) +
    R3.resolveY(u, v) +
    R3.resolveZ(u, v)
  }

  def module            =
    math.sqrt(x.square + y.square)

  def toR3: VectorR3    =
    VectorR3(x, y, 0)
}

/**
 * Vectors in R3
 */
case class VectorR3(x: Int, y: Int, z: Int) extends Vector {

  def +(that: VectorR3) =
    VectorR3(x + that.x, y + that.y, z + that.z)

  def -(that: VectorR3) =
    this + VectorR3(-that.x, -that.y, -that.z)

  def *(that: VectorR3) =
    x * that.x + y * that.y + z * that.z
  def *(that: Int)      =
    VectorR3(x * that, y * that, z * that)

  def module =
    math.sqrt(x.square + y.square + z.square )
}

object R3 {
  lazy val i = VectorR3(1, 0, 0)
  lazy val j = VectorR3(0, 1, 0)
  lazy val k = VectorR3(0, 0, 1)

  def resolveX(u: VectorR3, v: VectorR3) =
    i * (u.y * v.z - (v.y * u.z))
  def resolveY(u: VectorR3, v: VectorR3) =
    (j * -1) * (u.x * v.z - (v.x * u.z))
  def resolveZ(u: VectorR3, v: VectorR3) =
    k * (u.x * v.y - (v.x * u.y))
}

