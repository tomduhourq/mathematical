package utils

import complex.Cpx

/**
 * This object provides additional operations for number types.
 */
object NumberUtils {
  implicit class RichInt(val n: Int) extends AnyVal {
    def square = n * n
    def abs = if(n < 0) -n else n
    def ! = {
      def recFactorial(acum: BigInt, left: Int): BigInt =
        if(left == 1) acum
        else recFactorial(acum * left, left - 1)
      recFactorial(BigInt(1),n)
    }
    def *(that: vectors.Vector) = that * n
    def +(that: Cpx)            = Cpx(n + that.r, that.z)
  }
  implicit class RichDouble(val n: Double) extends AnyVal {
    def square = n * n
    def abs = if(n < 0) -n else n
    def ! = {
      def recFactorial(acum: BigInt, left: Int): BigInt =
        if(left == 1) acum
        else recFactorial(acum * left, left - 1)
      recFactorial(BigInt(1),n.toInt)
    }
    def *(that: vectors.Vector) = that * n
    def +(that: Cpx)            = Cpx(n + that.r, that.z)
  }
}


