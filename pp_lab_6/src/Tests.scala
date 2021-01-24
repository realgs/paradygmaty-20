import ParallelAlgorithms.{parMatrixProduct, parPrimes, parQuickSort}
import SequentialAlgorithms.{matrixProduct, matrixToString, primes, quickSort}

import scala.util.Random

object Tests {
  def timeMeasure(algorithm: () => Any): Long = {
    val t0 = System.currentTimeMillis()
    algorithm()
    System.currentTimeMillis() - t0
  }
  def main(args: Array[String]): Unit = {
    //Primes:
    println(primes(0) == List())
    println(primes(-2) == List())
    println(primes(29) == List(2, 3, 5, 7, 11, 13, 17, 19, 23))
    println(primes(40) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37))
    println(parPrimes(-2) == List())
    println(parPrimes(10) == List(2, 3, 5, 7))
    println(parPrimes(40) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37))
    println(parPrimes(70) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67))
    //Efficiency comparison:
    println(timeMeasure(() => primes(10000000)) + " ms")    // result = 6622 ms
    println(timeMeasure(() => parPrimes(10000000)) + " ms") // result = 2255 ms

    //Matrix:
    val a1 = Array(
      Array[Double](1, 2, 3),
      Array[Double](1, 2, 3),
      Array[Double](1, 2, 3)
    )
    val b1 = Array(
      Array[Double](3, 3, 3),
      Array[Double](2, 2, 2),
      Array[Double](1, 1, 1)
    )
    val res1 = Array(
      Array[Double](10, 10, 10),
      Array[Double](10, 10, 10),
      Array[Double](10, 10, 10)
    )
    val a2 = Array(
      Array[Double](2, 3),
      Array[Double](5, 6)
    )
    val b2 = Array(
      Array[Double](1, 0),
      Array[Double](0, 0)
    )
    val res2 = Array(
      Array[Double](2, 0),
      Array[Double](5, 0)
    )
    println(matrixToString(matrixProduct(a1, b1)) == matrixToString(res1))
    println(matrixToString(parMatrixProduct(a1, b1)) == matrixToString(res1))
    println(matrixToString(matrixProduct(a2, b2)) == matrixToString(res2))
    println(matrixToString(parMatrixProduct(a2, b2)) == matrixToString(res2))
    try {
      matrixProduct(a1, b2)
    } catch {
      case e: IllegalArgumentException => e.printStackTrace()
    }
    try {
      parMatrixProduct(a2, b1)
    } catch {
      case e: IllegalArgumentException => e.printStackTrace()
    }
    //Efficiency comparison:
    val r = Random
    val m1 = Array.fill(1000)(Array.fill(1000)(r.nextDouble()))
    val m2 = Array.fill(1000)(Array.fill(1000)(r.nextDouble()))
    println(timeMeasure(() => matrixProduct(m1, m2)) + " ms")    // result = 4583 ms
    println(timeMeasure(() => parMatrixProduct(m1, m2)) + " ms") // result = 1169 ms

    //Quick Sort:
    val array1 = Array[Double](6, 1, 55, 2, 5, -3, 11, 43, 10, 6, -10, 0, 2)
    val array2 = Array[Double](6, 1, 55, 2, 5, -3, 11, 43, 10, 6, -10, 0, 2)
    quickSort(array1)
    println(array1.toList == List(-10, -3, 0, 1, 2, 2, 5, 6, 6, 10, 11, 43, 55))
    parQuickSort(array2)
    println(array2.toList == List(-10, -3, 0, 1, 2, 2, 5, 6, 6, 10, 11, 43, 55))
    //Efficiency comparison:
    val ar1 = Array.fill(100000000)(r.nextDouble)
    val ar2 = ar1.clone()
    println(timeMeasure(() => quickSort(ar1)) + " ms")    // result = 15363 ms
    println(timeMeasure(() => parQuickSort(ar2)) + " ms") // result = 11224 ms
  }
}

