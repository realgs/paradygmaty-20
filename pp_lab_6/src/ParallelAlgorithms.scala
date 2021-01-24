import SequentialAlgorithms.{initializeMatrix, isPrime, rowsMultiplication, innerQuickSort, partition}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ParallelAlgorithms {

  //Matrix Product
  def parMatrixProduct(matrix1: Array[Array[Double]], matrix2: Array[Array[Double]]): Array[Array[Double]] = {
    val product = initializeMatrix(matrix1, matrix2)
    val avProcessors = Runtime.getRuntime.availableProcessors()
    val step = Math.ceil(matrix1.length.toDouble / avProcessors).toInt
    var futures: List[Future[Any]] = List()

    for(i <- Range(0, avProcessors)) {
      val left = i * step
      val right = Math.min(matrix1.length, (i + 1) * step)
      futures = Future(rowsMultiplication(matrix1, matrix2, product, left, right)) :: futures
    }
    futures.foreach(future => Await.ready(future, Duration.Inf))
    product
  }

  //Primes Generator
  def parPrimes(limit: Int): List[Int] = {
    val avProcessors = Runtime.getRuntime.availableProcessors()
    val step = Math.ceil(limit.toDouble / avProcessors).toInt
    var futures: List[Future[List[Int]]] = List()

    for (i <- Range(0, avProcessors)) {
      val left = 1 + i * step
      val right = Math.min(limit, 1 + (i + 1) * step)
      futures = Future(getPrimesInRange(left, right)) :: futures
    }

    val partialResults: List[List[Int]] = Await.result(Future.sequence(futures), Duration.Inf)
    partialResults.foldRight(List[Int]())((a: List[Int], b: List[Int]) => b ::: a)
  }

  private def getPrimesInRange(left: Int, right: Int): List[Int] = {
    var result = List[Int]()
    for (num <- Range(left, right)) {
      if (isPrime(num)) {
        result = num :: result
      }
    }
    result.reverse
  }

  //Quick Sort
  def parQuickSort(array: Array[Double]): Unit = {
      parInnerQuickSort(array, 0, array.length - 1)
    }

  private def parInnerQuickSort(array: Array[Double], begin: Int, end: Int): Unit = {
    if (begin < end) {
      val pivotIdx = partition(array, begin, end)
      val size = end - begin + 1
      val avProcessors = Runtime.getRuntime.availableProcessors()

      if (size <= array.length / avProcessors) {
        innerQuickSort(array, begin, pivotIdx - 1)
        innerQuickSort(array, pivotIdx + 1, end)
      } else {
        val left = Future(parInnerQuickSort(array, begin, pivotIdx - 1))
        parInnerQuickSort(array, pivotIdx + 1, end)
        Await.ready(left, Duration.Inf)
      }
    }
  }

}

