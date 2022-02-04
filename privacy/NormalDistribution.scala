package privacy

import scala.math.{Pi, cos, exp, log, random, sin, sqrt}
import scala.util.Random

class NormalDistribution(val sigma: Double) {
  def density(x: Double, y: Double): Double = exp(-(x * x + y * y) / (2 * sigma * sigma)) / (2 * Pi * sigma * sigma)

  /*          Method: quadratic fitting
   * Assume the quadratic function:
   *  z = a * x * x + b * x * y + c * y * y + d
   * The integral is:
   *  V = (x1 * x1 + x2 * x1 + x2 * x2) * a * S / 3
   *    + (x1 + x2) * (y1 + y2) * b * S / 4
   *    + (y1 * y1 + y2 * y1 + y2 * y2) * c * S / 3
   *    + d * S
   * where S = dx * dy
   *      dx = x2 - x1
   *      dy = y2 - y1
   */
  def computeProb(x1: Double, x2: Double, y1: Double, y2: Double): Double = {
    val z1 = density(x1, y1)
    val z2 = density(x1, y2)
    val z3 = density(x2, y1)
    val z4 = density(x2, y2)

    if (x1 + x2 ==0 && y1 + y2 == 0) {
      return (x2 - x1) * (y2 - y1) * z1
    }
    if (x1 + x2 == 0) {
      val CS = (z2 - z1) * (x2 - x1) / (y1 + y2)
      val DS = (z1 * y2 * y2 - z2 * y1 * y1) * (x2 - x1) / (y1 + y2)
      return (y1 * y1 + y1 * y2 + y2 * y2) * CS / 3 + DS
    }
    if (y1 + y2 == 0) {
      val AS = (z3 - z1) * (y2 - y1) / (x1 + x2)
      val DS = (z1 * x2 * x2 - z3 * x1 * x1) * (y2 - y1) / (x1 + x2)
      return (x1 * x1 + x1 * x2 + x2 * x2) * AS / 3 + DS
    }
    val AS = (-y2 * z1 + y1 * z2 + y2 * z3 - y1 * z4) / (x1 + x2) // AS = a * S
    val BS = z1 - z2 - z3 + z4                                    // BS = b * S
    val CS = (-x2 * z1 + x2 * z2 + x1 * z3 - x1 * z4) / (y1 + y2) // CS = c * S
    val DS = (  x2 * x2 * y2 * y2 * z1
              - x2 * x2 * y1 * y1 * z2
              - x1 * x1 * y2 * y2 * z3
              + x1 * x1 * y1 * y1 * z4
              - x1 * x2 * y1 * y2 * (z1 - z2 - z3 + z4)
             ) / ((x1 + x2) * (y1 + y2))
    return (x1 * x1 + x1 * x2 + x2 * x2) * AS / 3 + (x1 + x2) * (y1 + y2) * BS / 4 + (y1 * y1 + y1 * y2 + y2 * y2) * CS / 3 + DS
  }

  // linear fitting
  def testProb(x1: Double, x2: Double, y1: Double, y2: Double): Double = {
    val z1 = density(x1, y1)
    val z2 = density(x2, y2)
    val dx = x2 - x1
    val dy = y2 - y1

    return (z1 + z2) * dx * dy / 2
  }

  def sample: (Double, Double) = {
    val r = sigma * sqrt(-2 * log(1 - random()))
    val theta = 2 * Pi * random()
    val x = r * cos(theta)
    val y = r * sin(theta)
    (x, y)
  }
}
