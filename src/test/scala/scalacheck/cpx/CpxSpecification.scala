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
  property("commutativity of sum") = forAll(complex, complex) {
    (x: Cpx, y: Cpx) =>
      x + y == y + x
  }

  property("associativity of sum") = forAll(complex, complex, complex) {
    (x: Cpx, y: Cpx, z: Cpx) =>
      (x + y) + z == x + (y + z)
  }

  property("existence of neuter element") = forAll(complex) {
    (x: Cpx) =>
      x + Cpx(0, 0) == x
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

  property("non commutativity of subtraction") = forAll(complex, complex) {
    (x: Cpx, y: Cpx) =>
      x - y != y - x
  }

  property("non associativity of subtraction") = forAll(complex, complex, complex) {
    (x: Cpx, y: Cpx, z: Cpx) =>
      (x - y) - z != x - (y - z)
  }

  // Product
  property("commutativity of product") = forAll(complex, complex) {
    (x: Cpx, y: Cpx) =>
      x * y == y * x
  }

  property("associativity of product") = forAll(complex, complex, complex) {
    (x: Cpx, y: Cpx, z: Cpx) =>
      x * (y * z) == (x * y) * z
  }

  property("existence of neuter element") = forAll(complex) {
    (x: Cpx) =>
      x * Cpx(1, 0) == x
  }
}
