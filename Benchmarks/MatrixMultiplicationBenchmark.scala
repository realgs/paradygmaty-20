// Konrad Karanowski
package Benchmarks

import org.scalameter._
import MatrixMultiplication.{matmulParallel, matmulSequential}
import scala.util.Random

object MatrixMultiplicationBenchmark
{

  private def testAlgorithm(multiplication: (Array[Array[Double]], Array[Array[Double]]) => Array[Array[Double]])(
                           matrix1: Array[Array[Double]],
                           matrix2: Array[Array[Double]],
                           runsPerMatrix: Int,
                           verbose: Boolean = false
  ): Double =
  {
    val time = config(
      Key.exec.maxWarmupRuns -> runsPerMatrix,
      Key.verbose -> verbose,
    ) withWarmer(new Warmer.Default) measure
      {
        multiplication(matrix1, matrix2)
      }
    time.value
  }

  private def testRun(shapeA: (Int, Int), shapeB: (Int, Int), runsPerMatrix: Int): (Double, Double) =
  {
    val matrix1 = Array.fill(shapeA._1, shapeA._2)(Random.nextDouble())
    val matrix2 = Array.fill(shapeB._1, shapeB._2)(Random.nextDouble())
    (
      testAlgorithm(matmulSequential)(matrix1, matrix2, runsPerMatrix),
      testAlgorithm(matmulParallel)(matrix1, matrix2, runsPerMatrix)
    )
  }

  def testMatrixMultiplication(differentRuns: Int, shapeA: (Int, Int),
                               shapeB: (Int, Int), runsPerMatrix: Int): Unit =
  {
    var parallelTotalTime = .0
    var sequentialTotalTime = .0
    for (_ <- 0 to differentRuns)
    {
      val (sequentialTime, parallelTime) = testRun(shapeA, shapeB, runsPerMatrix)
      parallelTotalTime += parallelTime
      sequentialTotalTime += sequentialTime
    }
    println(f"Different runs: $differentRuns, Matrix A shape: $shapeA, Matrix B shape: $shapeB Runs per array: $runsPerMatrix")
    println(f"Mean time for the sequential solution: ${sequentialTotalTime / differentRuns}ms")
    println(f"Mean time for the parallel solution: ${parallelTotalTime / differentRuns}ms")
    println(f"Ratio sequential/parallel: ${sequentialTotalTime / parallelTotalTime}")
  }
}
