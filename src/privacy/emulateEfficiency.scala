package privacy

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import scala.math.{random, sqrt}

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
  def sigma(k: Int): Double = sqrt(k * (2 * ln_20 + epsilon)) * r_ind / epsilon

  override def run(): Unit = {
    val writer = new PrintWriter(new FileWriter("R=%d-efficacy-epsilon=%.1f-r=%d.csv".format(R, epsilon, r_ind)))
//    val distributions = for (k <- Array.range(1, 11)) yield {
//      new NormalDistribution(sigma(k))
//    }
    for (k <- 1 to 10) {
      val tryNum = k * baseTryNum
      val distribution = new NormalDistribution(sigma(k))
      for (w <- 0 until tryNum) {
        if (w % (tryNum / 100) == 0) {
          val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
          println(current)
          println("efficacy-R=%d-eps=%.1f-r=%d, k=%d: %d%%".format(R, epsilon, r_ind, k, w * 100 / tryNum))
        }
        val dummies = for (i <- Array.range(0, k)) yield {
          distribution.sample
        }
        val minX = dummies.map(dummy => dummy._1).min - R
        val maxX = dummies.map(dummy => dummy._1).max + R
        val minY = dummies.map(dummy => dummy._2).min - R
        val maxY = dummies.map(dummy => dummy._2).max + R
        val meanX = dummies.map(dummy => dummy._1).sum / dummies.length
        val meanY = dummies.map(dummy => dummy._2).sum / dummies.length
        val post_distribution = new NormalDistribution(sigma(1))
        val pdf = dummies.map(dummy => post_distribution.density(dummy._1 - meanX, dummy._2 - meanY))
        val sum = pdf.sum
        val prob = pdf.map(p => p / sum)

        var total = 0.0
        var effect = 0.0
        for (i <- 0 until sampleSize) {
          val deltaX = maxX - minX
          val deltaY = maxY - minY
          val x = random() * deltaX + minX
          val y = random() * deltaY + minY
          var possibility = 0.0
          for (j <- dummies.indices) {
            if ((dummies(j)._1 - x) * (dummies(j)._1 - x) + (dummies(j)._2 - y) * (dummies(j)._2 - y) <= R * R) {
              possibility += prob(j)
            }
          }
          total += possibility
          if (possibility != 0.0 && x * x + y * y <= R * R) {
            effect += possibility
          }
        }
        //println(total, effect)
        writer.println("%d,%f".format(k, effect / total))
      }
    }
    writer.close()
  }
}
