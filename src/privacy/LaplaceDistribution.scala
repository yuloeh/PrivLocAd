package privacy

import org.apache.commons.math3.distribution.{GammaDistribution, UniformRealDistribution}

import scala.math.{Pi, cos, sin}

class LaplaceDistribution(val b: Double) {
  def sample : (Double, Double) = {
    val gamma = new GammaDistribution(2, b)
    val r = gamma.sample()
    val uniform = new UniformRealDistribution(0, 2)
    val theta = uniform.sample() * Pi
    (r * cos(theta), r * sin(theta))
  }
}
