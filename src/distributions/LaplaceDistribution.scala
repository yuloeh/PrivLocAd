package distributions

import scala.math.{Pi, cos, random, sin, sqrt}

object LaplaceDistribution {
  def sample(scale: Double): (Double, Double) = {
    val sigma = sqrt(scale / 2)
    val (x1, x2) = NormalDistribution.sample(sigma)
    val (x3, x4) = NormalDistribution.sample(sigma)
    val r = x1 * x1 + x2 * x2 + x3 * x3 + x4 * x4   // complies with Gamma Distribution
    val theta = 2 * Pi * random()
    (r * cos(theta), r * sin(theta))
  }
}
