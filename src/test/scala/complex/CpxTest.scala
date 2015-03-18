package complex

import org.scalatest.{Matchers, FreeSpec}

class CpxTest extends FreeSpec with Matchers {
  "My little complex library" - {
    val c1 = Cpx(1, 1)
    "Sums 2 complex numbers" in {
      c1 + c1 should be (Cpx(2,2))
    }
  }
}
