package privacy

import dataset.{Task, TraceBuffer}
import distributions.NormalDistribution

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import scala.math.{Pi, cos, random, sin, sqrt}

object minProb {
  val tryNum = 100000
  val sampleSize = 10000

  def main(args: Array[String]): Unit = {
    val R = 5000
    val epsilon = 1
    val threads = for (r_ind <- Array.range(500, 1100, 100)) yield {
      new Thread(new minProb(tryNum, sampleSize, R, epsilon, r_ind))
    }

    for (thread <- threads) {
      thread.start()
    }
    for (thread <- threads) {
      thread.join()
    }
  }

}

class minProb(baseTryNum: Int, sampleSize: Int, R: Int, epsilon: Double, r_ind: Int) extends Runnable {
  def sigma(k: Int): Double = sqrt(k * (2 * ln_20 + epsilon)) * r_ind / epsilon

  private val rate = r_ind match {
    case 500 => Array(0,0.2,0.2,0.5,0.6,0.7,0.8,0.9,0.9,1.0)
    case 600 => Array(0,0.4,0.6,0.8,0.9,0.9,1.1,1.2,1.2,1.3)
    case 700 => Array(0,0.8,1.0,1.0,1.1,1.2,1.2,1.3,1.4,1.5)
    case 800 => Array(0,0.8,1.0,1.1,1.3,1.3,1.4,1.4,1.5,1.6)
    case 900 => Array(0,1.0,1.2,1.3,1.4,1.4,1.6,1.6,1.6,1.7)
    case 1000 => Array(0,1.2,1.3,1.5,1.5,1.6,1.7,1.8,1.8,1.8)
  }

  override def run(): Unit = {
    val writer = new PrintWriter(new FileWriter("R=%d-minProb-epsilon=%.1f-r=%d-opt2stage.csv".format(R, epsilon, r_ind)))
    for (k <- 2 to 10) {
      val tryNum = baseTryNum
      val lambda = rate(k - 1)
      val distr0 = new NormalDistribution(sigma(1) * lambda / sqrt(lambda * lambda + 1))
      val distr1 = new NormalDistribution(sigma(k) / sqrt(lambda * lambda + 1))
      for (w <- 0 until tryNum) {
        if (w % (tryNum / 100) == 0) {
          val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
          println(current)
          println("minProb-R=%d-eps=%.1f-r=%d, k=%d: %d%%".format(R, epsilon, r_ind, k, w * 100 / tryNum))
        }

        val point = distr0.sample
        val dummies = for (i <- Array.range(0, k)) yield {
          val (x, y) = distr1.sample
          (x + point._1, y + point._2)
        }

        val meanX = dummies.map(dummy => dummy._1).sum / dummies.length
        val meanY = dummies.map(dummy => dummy._2).sum / dummies.length
        val post_distribution = new NormalDistribution(sigma(1))
        val pdf = dummies.map(dummy => post_distribution.density(dummy._1 - meanX, dummy._2 - meanY))
        val sum = pdf.sum
        val alpha = 0.00000001
        val prob = pdf.map(p => (p + alpha) / (sum + k * alpha))
//        for (p <- prob) println(p)
//        println("sum:", prob.sum)
//        val prob = pdf.map(p => 1.0 / k)

        var minProb = 1.0
        for (i <- 0 until sampleSize/10) {
          val r = R * sqrt(random())
          val theta = 2 * Pi * random()
          val x = r * cos(theta)
          val y = r * sin(theta)
          var possibility = 0.0
          var flag = false
          for (j <- dummies.indices) {
            if ((dummies(j)._1 - x) * (dummies(j)._1 - x) + (dummies(j)._2 - y) * (dummies(j)._2 - y) <= R * R) {
              possibility += prob(j)
              flag = true
            }
          }
          if (flag && possibility < minProb) {
            minProb = possibility
          }
        }
        writer.println("%d,%f,%f,%f".format(k, minProb, prob.min, sum))
      }
    }
    writer.close()
  }
}
