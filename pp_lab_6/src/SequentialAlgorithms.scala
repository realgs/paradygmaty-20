import scala.annotation.tailrec

object SequentialAlgorithms {

  //Matrix Product
  def matrixProduct(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val product = initializeMatrix(matrix1, matrix2)
    rowsMultiplication(matrix1, matrix2, product)
  }

  def initializeMatrix(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] =
    if (matrix1.length == matrix2(0).length) Array.ofDim[Double](matrix1.length, matrix2(0).length)
    else throw new IllegalArgumentException

  def rowsMultiplication(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]], result:Array[Array[Double]]): Array[Array[Double]] = {
    for(i <- matrix1.indices) {
      for(j <- matrix2(0).indices)
        for(k <- matrix2.indices)
          result(i)(j) = matrix1(i)(k) * matrix2(k)(j)
    }
    result
  }

  //Primes Generator
  def primes(limit: Int): List[Int] = {
    @tailrec
    def helper(limit: Int, n: Int, list: List[Int]): List[Int] = {
        if(n == limit) Nil
        else helper(limit, n + 1, if(isPrime(n)) n :: list else list)
    }
    helper(limit, 1, Nil).reverse
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
