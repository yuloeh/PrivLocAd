package privacy

import distributions.NormalDistribution

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import scala.math.{Pi, cos, random, sin, sqrt}

object emulateEfficiency {
  val tryNum = 100000
  val sampleSize = 10000

  def main(args: Array[String]): Unit = {
    val R = 5000
    val epsilon = 1
    val threads = for (r_ind <- Array.range(500, 1100, 100)) yield {
      new Thread(new emulateEfficiency(tryNum, sampleSize, R, epsilon, r_ind))
    }
    //val ind_radius = 1000

    //epsilon = 0.4, ind_radius = 1000
    //val rad = Array(23114.96, 21813.97, 20964.94, 20458.21, 20059.70, 19817.94, 19694.89, 19496.74, 19403.57, 19261.60)
    //epsilon = 1.2, ind_radius = 1000
    //val rad = Array(14420.96, 13600.87, 12933.35, 12326.22, 11834.3, 11344.74, 10954.1, 10611.55, 10307.65, 10037.56, 9821.35, 9622.38, 9454.27, 9305.76, 9176.49, 9057.55, 8969.25, 8856.01, 8771.88, 8711.38)
    for (thread <- threads) {
      thread.start()
    }
    for (thread <- threads) {
      thread.join()
    }
  }
}

class emulateEfficiency(baseTryNum: Int, sampleSize: Int, R: Int, epsilon: Double, r_ind: Int) extends Runnable {
  def sigma(k: Int): Double = sqrt(k * (2 * ln_100 + epsilon)) * r_ind / epsilon

  val rate_1_0: Array[Double] = r_ind match {
    case 500 => Array(0,0.2,0.2,0.5,0.6,0.7,0.8,0.9,0.9,1.0)
    case 600 => Array(0,0.4,0.6,0.8,0.9,0.9,1.1,1.2,1.2,1.2)
    case 700 => Array(0,0.8,1.0,1.0,1.1,1.2,1.2,1.3,1.4,1.3)
    case 800 => Array(0,0.8,1.0,1.1,1.3,1.3,1.4,1.4,1.5,1.6)
    case 900 => Array(0,1.0,1.2,1.3,1.4,1.4,1.6,1.6,1.6,1.7)
    case 1000 => Array(0,1.2,1.3,1.5,1.5,1.6,1.7,1.8,1.8,1.8)
  }

  val rate_1_5: Array[Double] = r_ind match {
    case 500 => Array(0,0.0,0.1,0.0,0.0,0.3,0.1,0.4,0.3,0.6)
    case 600 => Array(0,0.2,0.1,0.2,0.4,0.4,0.6,0.7,0.6,0.8)
    case 700 => Array(0,0.3,0.1,0.4,0.5,0.8,0.7,0.9,0.9,0.9)
    case 800 => Array(0,0.4,0.4,0.7,0.7,0.8,0.9,1.0,1.1,1.1)
    case 900 => Array(0,0.6,0.8,0.7,0.9,0.9,1.0,1.2,1.2,1.3)
    case 1000 => Array(0,0.7,0.9,0.9,1.2,1.1,1.3,1.3,1.4,1.4)
  }

  val rate: Array[Double] = rate_1_0

  override def run(): Unit = {
//    val writer = new PrintWriter(new FileWriter("R=%d-efficacy-epsilon=%.1f-r=%d-opt2stage-balance.csv".format(R, epsilon, r_ind)))
    val writer1 = new PrintWriter(new FileWriter("R=%d-efficacy-epsilon=%.1f-r=%d-40-area.csv".format(R, epsilon, r_ind)))
//    val distributions = for (k <- Array.range(1, 11)) yield {
//      new NormalDistribution(sigma(k))
//    }
    for (k <- 40 to 41) {
      val tryNum = baseTryNum
//      val lambda = rate(k - 1)
      val lambda = 2.2
      val distr0 = new NormalDistribution(sigma(1) * lambda / sqrt(lambda * lambda + 1))
      val distr1 = new NormalDistribution(sigma(k) / sqrt(lambda * lambda + 1))
      for (w <- 0 until tryNum) {
        if (w % (tryNum / 100) == 0) {
          val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
          println(current)
          println("efficacy-R=%d-eps=%.1f-r=%d, k=%d: %d%%".format(R, epsilon, r_ind, k, w * 100 / tryNum))
        }

        val point = distr0.sample
        val dummies = for (i <- Array.range(0, k)) yield {
          val (x, y) = distr1.sample
          (x + point._1, y + point._2)
        }

        val minX = dummies.map(dummy => dummy._1).min - R/2
        val maxX = dummies.map(dummy => dummy._1).max + R/2
        val minY = dummies.map(dummy => dummy._2).min - R/2
        val maxY = dummies.map(dummy => dummy._2).max + R/2
        val meanX = dummies.map(dummy => dummy._1).sum / dummies.length
        val meanY = dummies.map(dummy => dummy._2).sum / dummies.length
        val post_distribution = new NormalDistribution(sigma(1))
        val pdf = dummies.map(dummy => post_distribution.density(dummy._1 - meanX, dummy._2 - meanY))
        val sum = pdf.sum
        val alpha = 0.00000001
//        val prob = pdf.map(p => (p + alpha) / (sum + k * alpha))
        val prob = pdf.map(p => 1.0 / k)

        var count = 0
        var total = 0.0
        var effect = 0.0
        val deltaX = maxX - minX
        val deltaY = maxY - minY
        for (i <- 0 until sampleSize/10) {
          val x = random() * deltaX + minX
          val y = random() * deltaY + minY
          var possibility = 0.0
          for (j <- dummies.indices) {
            if ((dummies(j)._1 - x) * (dummies(j)._1 - x) + (dummies(j)._2 - y) * (dummies(j)._2 - y) <= R * R / 4) {
              possibility += prob(j)
            }
          }
          if (possibility != 0.0) {
            count += 1
            total += possibility
            if (x * x + y * y <= R * R) effect += possibility
          }
        }
        //println(total, effect)
//        writer.println("%d,%f".format(k, effect / total))
        writer1.println("%d,%f".format(k, count.toDouble / (sampleSize/10) * deltaX * deltaY))
      }
    }
//    writer.close()
    writer1.close()
  }
}
