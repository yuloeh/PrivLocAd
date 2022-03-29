package privacy

import distributions.NormalDistribution

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import scala.math.{Pi, cos, random, sin, sqrt}

object baselineEfficacy {
  val tryNum = 100000
  val sampleSize = 10000

  def main(args: Array[String]): Unit = {
    val R = 5000
    val epsilon = 1
    val threads = for (r_ind <- Array.range(500, 600, 100)) yield {
      new Thread(new baselineEfficacy(tryNum, sampleSize, R, epsilon, r_ind))
    }
    for (thread <- threads) {
      thread.start()
    }
    for (thread <- threads) {
      thread.join()
    }
  }
}

class baselineEfficacy(baseTryNum: Int, sampleSize: Int, R: Int, epsilon: Double, r_ind: Int) extends Runnable {
  private def sigma(k: Int) = sqrt(k * (2 * ln_100 + epsilon)) * r_ind / epsilon

  val Rxes = Array(5000,5752,6279,6687,7003,7275,7514,7705,7872,8022)

  override def run(): Unit = {
    val writer = new PrintWriter(new FileWriter("R=%d-efficacy-epsilon=%.1f-r=%d-baseline.csv".format(R, epsilon, r_ind)))
    for (k <- 1 to 10) {
      val tryNum = baseTryNum
      val Rx = Rxes(k - 1)
      val distr = new NormalDistribution(sigma(1))
      for (w <- 0 until tryNum) {
        if (w % (tryNum / 100) == 0) {
          val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
          println(current)
          println("efficacy-R=%d-eps=%.1f-r=%d, k=%d: %d%%".format(R, epsilon, r_ind, k, w * 100 / tryNum))
        }

        val point = distr.sample
        var effect = 0
        for (i <- 0 until sampleSize / 10) {
          val r = Rx * sqrt(random())
          val theta = 2 * Pi * random()
          val x = point._1 + r * cos(theta)
          val y = point._2 + r * sin(theta)
          if (x * x + y * y <= R * R) effect += 1
        }
        writer.println("%d,%f".format(k, effect.toDouble / (sampleSize / 10)))
      }
    }
  }
}