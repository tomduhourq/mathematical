package vectors

import org.scalatest.{FreeSpec, Matchers}
import utils.NumberUtils._

class VectorsTest extends FreeSpec with Matchers {

  "My little vector library" - {
    "Makes these operations in:" - {
      "R2" - {
        val u = V2(2, 5)
        val v = V2(3, 4)
        val w = V2(-300, 1)
        val x = V2(1,180)
        "Adds two vectors" in {
          u + v should be (V2(5, 9))
          v + w should be (V2(-297, 5))
          w + u should be (V2(-298, 6))
        }
        "Subtracts two vectors" in {
          u - v should be (V2(-1, 1))
          v - w should be (V2(303, 3))
          w - u should be (V2(-302, -4))
        }
        "Correctly performs scalar products" in {
          u * v should be (26)
          v * w should be (-896)
          w * u should be (-595)
          w * 2 should be (V2(-600, 2))
          2 * w should be (V2(-600, 2))
        }
        "Correctly performs cross products" in {
          u x v should be (V3(0, 0, -7))
          v x w should be (V3(0, 0, 1203))
          w x v should be (V3(0, 0, -1203))
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
      "R3" - {
        val u = V3(2, 5, 3)
        val v = V3(3, 4, 7)
        val w = V3(-300, 1, 2)
        "Adds two vectors" in {
          u + v should be (V3(5, 9, 10))
          v + w should be (V3(-297, 5, 9))
          w + u should be (V3(-298, 6, 5))
        }
        "Subtracts two vectors" in {
          u - v should be (V3(-1, 1, -4))
          v - w should be (V3(303, 3, 5))
          w - u should be (V3(-302, -4, -1))
        }
        "Correctly performs scalar products" in {
          u * v should be (47)
          v * w should be (-882)
          w * u should be (-589)
          w * 2 should be (V3(-600, 2, 4))
          2 * w should be (V3(-600, 2, 4))
        }
        "Correctly performs cross products" in {
          u x v should be (V3(23, -5, -7))
          v x w should be (V3(1, -2106, 1203))
          w x v should be (V3(-1, 2106, -1203))
        }
        "Resolves modules" in {
          u.magnitude should be (6.164414002968976)
          v.magnitude should be (8.602325267042627)
          w.magnitude should be (300.0083332175958)
        }
        "Projects correctly 2 vectors" in {
          u.projection(v) should be (0.6351351351351351)
          v.projection(u) should be (1.2368421052631582)
        }
      }
    }
  }
}
