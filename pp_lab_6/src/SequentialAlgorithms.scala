import scala.util.Random

object SequentialAlgorithms {

  //Matrix Product
  def matrixProduct(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val product = initializeMatrix(matrix1, matrix2)
    rowsMultiplication(matrix1, matrix2, product, 0, product.length)
  }

  def initializeMatrix(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] =
    if (matrix1.length == matrix2(0).length) Array.fill(matrix1.length)(Array.fill(matrix2(0).length)(0))
    else throw new IllegalArgumentException

  def rowsMultiplication(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]], result:Array[Array[Double]], beginRow: Int, endRow:Int): Array[Array[Double]] = {
    for(i <- Range(beginRow, endRow)) {
      for(j <- matrix2(0).indices)
        for(k <- matrix1(0).indices)
          result(i)(j) = result(i)(j) + matrix1(i)(k) * matrix2(k)(j)
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

  //Quick Sort
  def quickSort(array: Array[Double]): Unit = {
    innerQuickSort(array, 0, array.length - 1)
  }

  def innerQuickSort(array: Array[Double], begin: Int, end: Int): Unit = {
    if (begin < end) {
      val pivotIdx = partition(array, begin, end)
      innerQuickSort(array, begin, pivotIdx - 1)
      innerQuickSort(array, pivotIdx + 1, end)
    }
  }

  def partition(array: Array[Double], begin: Int, end: Int): Int = {
    val randIdx = Random.nextInt(end - begin) + begin
    swap(array, randIdx, end)
    val pivot = array(end)
    var i = begin - 1
    for (j <- Range(begin, end)) {
      if (array(j) <= pivot) {
        i = i + 1
        swap(array, i, j)
      }
    }
    swap(array, i + 1, end)
    i + 1
  }

  def swap(array: Array[Double], i: Int, j: Int): Unit = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }
}

