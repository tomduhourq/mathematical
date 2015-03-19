package scalacheck.cpx

import complex.Cpx
import utils.NumberUtils._
import org.scalacheck._
import Prop.forAll


object CpxSpecification extends Properties("Cpx") {

  // Generate random Cpx numbers
  val complex: Gen[Cpx] = for {
      r <- Gen.choose(-500, 500)
      z <- Gen.choose(-500, 500)
    } yield Cpx(r, z)

  val doubs: Gen[Double] = Gen.choose(-100, 100)

  // Sum 2 Complex
  property("commutativity of sum (2 Cpx)") = forAll(complex, complex) {
    (x: Cpx, y: Cpx) =>
      x + y == y + x
  }

  property("associativity of sum (3 Cpx)") = forAll(complex, complex, complex) {
    (x: Cpx, y: Cpx, z: Cpx) =>
      (x + y) + z == x + (y + z)
  }

  // Sums a complex and a number
  property("commutativity of sum (1 Cpx 1 Num)") = forAll(complex, doubs) {
    (x: Cpx, y: Double) =>
      x + y == y + x
  }

  // Generating doubles caused inconsistencies because of
  // scala's Double + precision operator
  property("associativity of sum (1 Cpx 2 Num)") = forAll(complex, doubs, doubs) {
    (x: Cpx, y: Double, z: Double) =>
      (x + y.toInt) + z.toInt == x + (y.toInt + z.toInt)
  }

  property("Non commutativity of subtraction") = forAll(complex, complex) {
    (x: Cpx, y: Cpx) =>
      x - y != y - x
  }

  property("Non associativity of subtraction") = forAll(complex, complex, complex) {
    (x: Cpx, y: Cpx, z: Cpx) =>
      (x - y) - z != x - (y - z)
  }
}
