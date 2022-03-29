package privacy

import distributions.NormalDistribution

import java.io.{FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import scala.math.{Pi, cos, random, sin, sqrt}

object TwoStage {
  val tryNum = 100000
  val sampleSize = 10000

  def main(args: Array[String]): Unit = {
    val R = args(0).toInt
    val epsilon = args(1).toDouble
    val r_ind = args(2).toInt

    for (lambda1 <- Array.range(1, 26)) {
      val threads = (for (r_ind <- Array.range(500, 1100, 100)) yield {
        new Thread(new privacy.twoStageEstimator(tryNum, sampleSize, R, epsilon, r_ind, Array(2, 3), lambda1, 10))
      }) ++ (for (r_ind <- Array.range(500, 1100, 100)) yield {
        new Thread(new privacy.twoStageEstimator(tryNum, sampleSize, R, epsilon, r_ind, Array(4, 5), lambda1, 10))
      }) ++ (for (r_ind <- Array.range(500, 1100, 100)) yield {
        new Thread(new privacy.twoStageEstimator(tryNum, sampleSize, R, epsilon, r_ind, Array(6, 7), lambda1, 10))
      }) ++ (for (r_ind <- Array.range(500, 1100, 100)) yield {
        new Thread(new privacy.twoStageEstimator(tryNum, sampleSize, R, epsilon, r_ind, Array(8, 9), lambda1, 10))
      }) ++ (for (r_ind <- Array.range(500, 1100, 100)) yield {
        new Thread(new privacy.twoStageEstimator(tryNum * 2, sampleSize, R, epsilon, r_ind, Array(10), lambda1, 10))
      })
      for (thread <- threads) {
        thread.start()
      }
      for (thread <- threads) {
        thread.join()
      }
    }
  }
}

class TwoStage(baseTryNum: Int, sampleSize: Int, R: Int, epsilon: Double, r_ind: Int, k_range: Array[Int], lambda1: Int = 1, lambda2: Int = 1) extends Runnable {
  override def run(): Unit = {
    def sigma(k: Int) = sqrt(k * (2 * ln_100 + epsilon)) * r_ind / epsilon
    //        var covered = 0
    //    val lambda = pow(2.0, log_lambda / 10.0)
    for (k <- k_range) {
      //      val writer = new PrintWriter(new FileWriter("%d/R=%d-coverage-epsilon=%.1f-rind=%d-log_lambda=%d-2stage.csv".format(k, R, epsilon, r_ind, log_lambda), true))
      //      val distr0 = new NormalDistribution(sigma(1) * lambda / sqrt(1 + lambda * lambda))
      //      val distr1 = new NormalDistribution(sigma(k) * 1 / sqrt(1 + lambda * lambda))
      val writer = new PrintWriter(new FileWriter("%d/R=%d-coverage-epsilon=%.1f-rind=%d-2stage-%d-%d.csv".format(k, R, epsilon, r_ind, lambda1, lambda2), true))
      val distr0 = new NormalDistribution(sigma(1) * lambda1 / sqrt(lambda1 * lambda1 + lambda2 * lambda2))
      val distr1 = new NormalDistribution(sigma(k) * lambda2 / sqrt(lambda1 * lambda1 + lambda2 * lambda2))
      val tryNum = baseTryNum
      //val tryNum = k * baseTryNum
      //val tryNum = k * k * baseTryNum
      for (w <- 0 until tryNum) {
        if (w % (tryNum / 100) == 0) {
          val current = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(System.currentTimeMillis())
          println(current)
          //          println("2stage-R=%d-eps=%.1f-r=%d, log_lambda=%d, k=%d: %d%%".format(R, epsilon, r_ind, log_lambda, k, w * 100 / tryNum))
          println("2stage-R=%d-eps=%.1f-r=%d, lambda1=%d, lambda2=%d k=%d: %d%%".format(R, epsilon, r_ind, lambda1, lambda2, k, w * 100 / tryNum))
        }

        val point = distr0.sample
        val dummies = for (i <- Array.range(0, k)) yield {
          val (x, y) = distr1.sample
          (x + point._1, y + point._2)
        }

        var covered = 0
        for (i <- 0 until sampleSize) {
          val r = R * sqrt(random())
          val theta = 2 * Pi * random()
          val x = r * cos(theta)
          val y = r * sin(theta)
          if (dummies.exists(p => sqrt((p._1 - x) * (p._1 - x) + (p._2 - y) * (p._2 - y)) <= R)) {
            covered += 1
          }
        }
        writer.println("%d,%d".format(k, covered))
      }
      writer.close()
    }
  }
}

