package complex

/** Immutable complex number
 * and its operations
 */
case class Cpx(r: Double, z: Double) {

  def +[@specialized(Int, Long) A](that: A)(implicit f: A => Double) =
    Cpx(that + r, z)

  def -[@specialized(Int, Long) A](that: A)(implicit f: A => Double) =
    Cpx(that - r, z)

  def +(that: Cpx) =
    Cpx(r + that.r, z + that.z)

  override def toString =
    s"$r + $z j"

  implicit def f[A] = (a:A) => a.toString.toDouble
}

object j extends Cpx(0, 1) {
  def value(pow: Double) = pow % 4 match {
    case 0 => Cpx(1, 0)
    case 1 => this
    case 2 => Cpx(-1, 0)
    case 3 => Cpx(0, -1)
  }
}
