package vectors

import org.scalatest.{FreeSpec, Matchers}
import utils.NumberUtils._

/**
 * Created by tomas on 14/03/15.
 */
class VectorsTest extends FreeSpec with Matchers {

  "My little vector library" - {
    "Makes these operations in:" - {
      "R2" - {
        val u = VectorR2(2, 5)
        val v = VectorR2(3, 4)
        val w = VectorR2(-300, 1)
        "Adds two vectors" in {
          u + v should be (VectorR2(5, 9))
          v + w should be (VectorR2(-297, 5))
          w + u should be (VectorR2(-298, 6))
        }
        "Subtracts two vectors" in {
          u - v should be (VectorR2(-1, 1))
          v - w should be (VectorR2(303, 3))
          w - u should be (VectorR2(-302, -4))
        }
        "Correctly performs scalar products" in {
          u * v should be (26)
          v * w should be (-896)
          w * u should be (-595)
          w * 2 should be (VectorR2(-600, 2))
          2 * w should be (VectorR2(-600, 2))
        }
        "Correctly performs cross products" in {
          u x v should be (VectorR3(0, 0, -7))
          v x w should be (VectorR3(0, 0, 1203))
          w x v should be (VectorR3(0, 0, -1203))
        }
        "Resolves modules" in {
          u.module should be (math.sqrt(29))
          v.module should be (5)
          w.module should be (math.sqrt(90001))
        }
      }
    }
  }
}
