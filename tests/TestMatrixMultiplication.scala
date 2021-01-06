// Adrian Chłopowiec
package tests

import algorithms.{MatrixMultiplication, MatrixMultiplicationPar}
import org.scalameter.measure

/*
  The second problem I've chosen is matrix multiplication. Being able to parallelize big matrixes is important factor in
  many computer programs. Similarly to previous problem - Quicksort, here for small matrixes Sequencial calculations are
  faster due to necessity of launching multiple threads in parallel algorithm.

  I've chosen to parallelize matrix multiplication in a way that I calculate rows of product matrix simultaneously.

  For example:
  n - matrix size

  n = 100x100
  Parallel time: 63.232 ms
  Seq time: 30.5977 ms

  n = 2000x2000
  Parallel time: 13544.2576 ms
  Seq time: 58459.5619 ms
 */
object TestMatrixMultiplication
{
  def main(args: Array[String]): Unit =
  {
    val random = scala.util.Random
    val left = Array.fill(100)(Array.fill(100)(random.nextInt(1000)))
    val right = Array.fill(100)(Array.fill(100)(random.nextInt(1000)))

    val seqTime = measure
    {
      MatrixMultiplication.multiply(left, right)
    }

    val parTime = measure
    {
      MatrixMultiplicationPar.multiplyPar(left, right)
    }

    println("Matrix multiplication")
    println("Parallel time: " + parTime)
    println("Seq time: " + seqTime)
  }
}
