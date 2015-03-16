package vectors

import org.scalatest.FlatSpec

class R3Test extends FlatSpec {

  behavior of "R3"

  it should "add a list of Vectors in R2" in {
    implicit val m = VMonoids.addMonoidV2
    val l = List(V2(1, 1) , V2(2, 2), V2(3, 3))
    assert(R3.sumV2(l) === V2(6, 6))
  }
}
