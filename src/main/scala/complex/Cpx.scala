package complex

import scala.math.BigDecimal
import utils.NumberUtils._

/** Immutable complex number
 * and its operations
 */
case class Cpx(r: Double, z: Double) {

  def +[@specialized(Int, Long) A](that: A)(implicit f: A => Double) =
    Cpx(that + r, z)

  def -[@specialized(Int, Long) A](that: A)(implicit f: A => Double) =
    Cpx(that - r, z)

  def *[@specialized(Int, Long) A](that: A)(implicit f: A => Double) =
    Cpx(r * that, z * that)

  def +(that: Cpx) =
    Cpx(r + that.r, z + that.z)

  def -(that: Cpx) =
    this + Cpx(-that.r, -that.z)

  def *(that: Cpx) =
    Cpx(
      r * that.r -(z * that.z),
      r * that.z + z * that.r
    )

  def /(that: Cpx): Cpx = that match {
    case Cpx(0, 0) =>
      throw new ArithmeticException("Cannot divide by zero")
    case Cpx(a, 0) =>
      Cpx(r / a, z / a)
    case Cpx(a, b) =>
      (this * conjugate(that)) /
      Cpx((that.r ^ 2) + (that.z ^ 2), 0)
  }

  lazy val squareRoot =
    Cpx(
      math.sqrt((r + length) / 2),
      z.signum * math.sqrt((r * -1 + length) / 2)
    )

  val length =
    math.sqrt((r ^ 2) + (z ^ 2))

  private def conjugate(c: Cpx) =
    Cpx(c.r, -c.z)

  override def toString =
    s"$r ${if(z < 0) "" else "+"} ${z}j"

  implicit def f[A]: (A) => Double =
    (a:A) =>
      BigDecimal(a.toString)
      .setScale(4, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
}

object j extends Cpx(0, 1) {
  def value(pow: Double) = pow % 4 match {
    case 0 => Cpx(1, 0)
    case 1 => this
    case 2 => Cpx(-1, 0)
    case 3 => Cpx(0, -1)
  }
}
