package vectors

import utils.NumberUtils._

/**
 * This is the general definition of a vector.
 * All other vectors extend from this, and must define the common operators.
 */
sealed trait Vector {
  def *(n: Double)         : Vector
  def *(v: Vector)         : Double
  def toR3                 : V3
  val length               : Double
  val normal               : Vector
  def projection(v: Vector) =
    this * v / v.length.square
}

object Vector {
  lazy val origin = V3.origin
}

/**
 * Vectors in R2
 */
case class V2(x: Double, y: Double) extends Vector {

  def +(that: Vector)         =
    that match {
      case V2(a, b)     => 
        V2(x + a, y + b)
      case V3(a, b , c) =>
        V3(x + a, y + b, c)
      case _ => V2.origin
    }

  def -(that: V2)             =
    this + V2(-that.x, -that.y)

  def *(that: Double)         =
    V2(x * that, y * that)
  def *(that: Vector)         =
    that match {
      case V2(a, b) =>
        x * a + y * b
      case _        =>
        throw new RuntimeException(
          "Cannot multiply the 2 types of Vectors"
        )
    }

  def x(that: V2)             = {
    val u = this.toR3
    val v = that.toR3
    R3.resolveX(u, v) +
    R3.resolveY(u, v) +
    R3.resolveZ(u, v)
  }

  lazy val length             =
    math.sqrt(x.square + y.square)

  def angle                   =
    1/math.tan(y/x)

  def toR3: V3                =
    V3(x, y, 0)

  lazy val normal             =
    V2(
      x / this.length,
      y / this.length
    )

  override def toString       =
    s"($x, $y)"
}

object V2 {
  lazy val origin = V2(0, 0)
}

/**
 * Vectors in R3
 */
case class V3(x: Double, y: Double, z: Double) extends Vector {

  def +(that: Vector): V3       =
    that match {
      case V2(a, b)    =>
        V3(x + a, y + b, z)
      case V3(a, b, c) =>
        V3(x + a, y + b, z + c)
      case _ => V3.origin
    }

  def -(that: V3)               =
    this + V3(-that.x, -that.y, -that.z)

  def *(that: Vector)           =
    that match {
      case V3(a, b, c) =>
        x * a + y * b + z * c
      case _           =>
        throw new RuntimeException(
          "Cannot multiply the 2 types of Vectors"
        )
    }
  def *(that: Double)           =
    V3(x * that, y * that, z * that)

  def x(that: V3)               =
    R3.resolveX(this, that) +
    R3.resolveY(this, that) +
    R3.resolveZ(this, that)

  lazy val length                    =
    math.sqrt(x.square + y.square + z.square )

  def toR3                      =
    this

  lazy val normal               =
    V3(
      x / this.length,
      y / this.length,
      z / this.length
    )

  override def toString         =
    s"($x, $y, $z)"
}

object V3 {
  lazy val origin = V3(0, 0, 0)
}

