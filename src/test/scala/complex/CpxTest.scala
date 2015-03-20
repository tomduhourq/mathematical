package complex

import org.scalatest.{Matchers, FreeSpec}
import utils.NumberUtils._

class CpxTest extends FreeSpec with Matchers {
  "My little complex library" - {
    val c1 = Cpx(1, 1)
    val c2 = j.value(23)
    "Adds 2 complex numbers" in {
      c1 + c1 should be (Cpx(2,2))
      c1 + c2 should be (Cpx(1,0))
    }
    "Adds a complex to a number" in {
      c1 + 2 should be (Cpx(3, 1))
      2 + c1 should be (Cpx(3, 1))
      c2 + 1 should be (Cpx(1, -1))
    }
    "Subtracts two Cpx" in {
      Cpx(5, -2) - Cpx(-4, -1) should be (Cpx(9, -1))
    }
    "Multiplies 2 Cpx" in {
      Cpx(2, -1) * Cpx(3, 4) should be (Cpx(10, 5))
      c1 * c2 should be (Cpx(1, -1))
    }
    "Divides 2 Cpx" in {
      Cpx(3, 0) / Cpx(2, 1) should be (Cpx(6/5.toDouble, -3/5.toDouble))
    }
  }
}
