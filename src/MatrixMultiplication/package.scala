// Konrad Karanowski
import scala.collection.parallel.CollectionConverters._

/*
  Naive algorithm for calculating matrix dot product
  In order to parallelize calculations, I used parallel for-loop.
  N jobs determine the level of parallelization (1 means sequential).
 */

package object MatrixMultiplication
{

  def matmulSequential(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] =
  {
    val rowsA = mat1.length
    val colsA = mat1(0).length
    val colsB = mat2(0).length
    val product = Array.fill(rowsA, colsB)(.0)
    for (i <- 0 until rowsA)
      for (j <- 0 until colsA)
        for (k <- 0 until colsB)
        {
          val calculation = mat1(i)(j) * mat2(j)(k)
          product(i)(k) += calculation
        }
    product
  }

  def matmulParallel(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] =
  {
    val nJobs = Runtime.getRuntime.availableProcessors()
    val nJobs1 = nJobs / 2
    val nJobs2 = nJobs - nJobs1
    val rowsA = mat1.length
    val colsA = mat1(0).length
    val colsB = mat2(0).length
    /*
    val nJobs1 = scala.math.sqrt(nJobs * rowsA / colsA).toInt
    val nJobs2 = nJobs / nJobs1
    */
    val product = Array.fill(rowsA, colsB)(.0)
    for (id1 <- (0 until nJobs1).par)
      for (i <- id1 * rowsA / nJobs1 until (id1 + 1) * rowsA / nJobs1)
        for (id2 <- (0 until nJobs2).par)
          for (j <- id2 * colsA / nJobs2 until (id2 + 1) * colsB / nJobs2)
            for (k <- 0 until colsB)
            {
              val calculation = mat1(i)(j) * mat2(j)(k)
              product(i)(k) += calculation
            }
    product
  }
}
