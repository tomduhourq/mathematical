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
  }
}
