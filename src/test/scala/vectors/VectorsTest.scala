package vectors

import org.scalatest.{FreeSpec, Matchers}
import utils.NumberUtils._

class VectorsTest extends FreeSpec with Matchers {

  "My little vector library" - {
    "Makes these operations in:" - {
      "R2" - {
        val u = VectorR2(2, 5)
        val v = VectorR2(3, 4)
        val w = VectorR2(-300, 1)
        val x = VectorR2(1,180)
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
          u.magnitude should be (math.sqrt(29))
          v.magnitude should be (5)
          w.magnitude should be (math.sqrt(90001))
        }
        "Computes the angle formed by its coordinates" in {
          u.angle should be (-1.3386481283041514)
          w.angle should be (-299.99888888806584)
          x.angle should be (0.7469988144140444)
        }
        "Projects correctly 2 vectors" in {
          u.projection(v) should be (1.04)
          v.projection(u) should be (0.8965517241379312)
        }
      }
    }
  }
}
