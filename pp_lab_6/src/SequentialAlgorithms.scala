import scala.annotation.tailrec

object SequentialAlgorithms {

  //Matrix Product
  def matrixProduct(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val product = initializeMatrix(matrix1, matrix2)
    rowsMultiplication(matrix1, matrix2, product, 0, matrix1(0).length)
  }

  def initializeMatrix(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] =
    if (matrix1.length == matrix2(0).length) Array.fill(matrix1.length)(Array.fill(matrix2(0).length)(0))
    else throw new IllegalArgumentException

  def rowsMultiplication(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]], result:Array[Array[Double]], beginRow: Int, endRow:Int): Array[Array[Double]] = {
    for(i <- Range(beginRow, endRow)) {
      for(j <- matrix2(0).indices)
        for(k <- matrix1(0).indices)
          result(i)(j) = matrix1(i)(k) * matrix2(k)(j)
    }
    result
  }


  def matrixToString(matrix: Array[Array[Double]]): String = {
    var s: String = "Matrix:"
    for(i <- matrix.indices) {
      s = s + "\n"
      for(j <- matrix(0).indices)
        s = s + matrix(i)(j) + " "
    }
    s
  }


  //Primes Generator
  def primes(limit: Int): List[Int] = {
    var result = List[Int]()
    for (num <- Range(1, limit))
      if (isPrime(num)) result = num :: result
    result.reverse
  }

  def isPrime(number: Int): Boolean =
    number match {
      case 1 => false
      case 2 => true
      case _ => var flag = true
        var counter = 2
        while(counter * counter <= number && flag) {
          flag = number % counter != 0
          counter += 1
        }
        flag
    }
}
