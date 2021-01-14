// Konrad Karanowski
package TestParallelization

import PathsShortestDistances.{pathsShortestDistancesSequential, pathsShortestDistancesParallel}
import MatrixMultiplication.{matmulSequential, matmulParallel}
import MonteCarloIntegration.{estimateIntegralSequential, estimateIntegralParallel}
import Quicksort.{quicksortSequential, quicksortParallel}

import scala.util.Random
import scala.math.{log, abs}

/*
  Testing, if parallel algorithms are giving the same results as sequential ones
 */

object TestParallelization
{

  private def compareMatrix[A](mat1: Array[Array[A]], mat2: Array[Array[A]]): Boolean =
  {
    for (i <- mat1.indices)
      for (j <- mat1(0).indices)
        if (mat1(i)(j) != mat2(i)(j)) false
    true
  }

  private def compareMatrixMultiplications(): Boolean =
  {
    val mat1 = Array.fill(1000, 1000)(Random.nextDouble())
    val mat2 = Array.fill(1000, 1000)(Random.nextDouble())
    compareMatrix(matmulSequential(mat1, mat2), matmulParallel(mat1, mat2))
  }

  private def compareQuicksort(): Boolean =
  {
    val array1 = Array.fill(1000)(Random.nextInt())
    val array2 = array1.clone()
    quicksortSequential(array1)
    quicksortParallel(array2)
    array1.sameElements(array2)
  }

  private def compareShortestPaths(): Boolean =
  {
    val mat1 = Array.fill(1000, 1000)(Random.nextInt())
    compareMatrix(pathsShortestDistancesSequential(mat1), pathsShortestDistancesParallel(mat1))
  }

  private def compareMonteCarlo(): Boolean =
  {
    val res1 = estimateIntegralSequential(log, .0, 2.78, 1_000_000)
    val res2 = estimateIntegralParallel(log, .0, 2.78, 1_000_000)
    abs(res1 - res2) <= 0.1
  }

  private def runTestInSequence(test: => Boolean, numSequences: Int): Boolean =
  {
    if (numSequences <= 0) true
    else test && runTestInSequence(test, numSequences - 1)
  }

  def main(args: Array[String]): Unit =
  {
    println(f"Matrix multiplication: ${runTestInSequence(compareMatrixMultiplications(), 10)}")
    println(f"Floyd-Warshall algorithm: ${runTestInSequence(compareShortestPaths(), 10)}")
    println(f"Quicksort: ${runTestInSequence(compareQuicksort(), 10)}")
    println(f"Monte carlo: ${runTestInSequence(compareMonteCarlo(), 10)}")
  }
}
