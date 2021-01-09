import scala.collection.parallel.CollectionConverters._

object MatrixMultiplication {
  def matrixMultiplication(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = matrix1.length
    val columns = matrix2(0).length
    val columns1rows2 = matrix2.length
    val product = Array.fill(rows, columns)(.0)

    for (i <- 0 until rows)
      for (j <- 0 until columns)
        for(k <- 0 until columns1rows2)
          product(i)(j) += matrix1(i)(k) * matrix2(k)(j)
    product
  }

  def matrixMultiplicationParallel(depth: Int)(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = matrix1.length
    val columns = matrix2(0).length
    val columns1rows2 = matrix2.length
    val product = Array.fill(rows, columns)(.0)

    for (n <- (0 until depth).par)
      for(i <- n * rows / depth until (n+1) * rows / depth)
        for (j <- 0 until columns)
          for(k <- 0 until columns1rows2)
            product(i)(j) += matrix1(i)(k) * matrix2(k)(j)
    product
  }
}
