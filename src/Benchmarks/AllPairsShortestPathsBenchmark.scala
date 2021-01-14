// Konrad Karanowski
package Benchmarks

import org.scalameter._
import PathsShortestDistances.{pathsShortestDistancesParallel, pathsShortestDistancesSequential}
import scala.math.abs
import scala.util.Random

object AllPairsShortestPathsBenchmark
{
  private def testAlgorithm(algorithm: Array[Array[Int]] => Array[Array[Int]])(
                           adjacencyMatrix: Array[Array[Int]],
                           runsPerGraph: Int,
                           verbose: Boolean = false
  ): Double =
  {
    val time = config(
      Key.exec.maxWarmupRuns -> runsPerGraph,
      Key.verbose -> verbose,
    ) withWarmer(new Warmer.Default) measure
      {
        algorithm(adjacencyMatrix)
      }
    time.value
  }

  private def generateAdjacencyMatrix(numVertex: Int): Array[Array[Int]] =
  {
    val adjacencyMatrix = Array.fill(numVertex, numVertex)(0)
    for (i <- 0 until numVertex)
      for  (j <- i until numVertex)
      {
        val randomNumber = abs(Random.nextInt())
        adjacencyMatrix(i)(j) = randomNumber
        adjacencyMatrix(j)(i) = randomNumber
      }
    adjacencyMatrix
  }

  private def testRun(numVertex: Int, runsPerGraph: Int): (Double, Double) =
  {
    val adjacencyMatrix = generateAdjacencyMatrix(numVertex)
    (
      testAlgorithm(pathsShortestDistancesSequential)(adjacencyMatrix, runsPerGraph),
      testAlgorithm(pathsShortestDistancesParallel)(adjacencyMatrix, runsPerGraph)
    )
  }

  def testAllShortestPaths(differentRuns: Int, numVertex: Int, runsPerGraph: Int): Unit =
  {
    var parallelTotalTime = .0
    var sequentialTotalTime = .0
    for (_ <- 0 until differentRuns)
    {
      val (sequentialTime, parallelTime) = testRun(numVertex, runsPerGraph)
      parallelTotalTime += parallelTime
      sequentialTotalTime += sequentialTime
    }
    println(f"Different runs: ${differentRuns}, Number of vertex in graph: ${numVertex}, Runs per graph: ${runsPerGraph}")
    println(f"Mean time for the sequential solution: ${sequentialTotalTime / differentRuns}ms")
    println(f"Mean time for the parallel solution: ${parallelTotalTime / differentRuns}ms")
    println(f"Ratio sequential/parallel: ${sequentialTotalTime / parallelTotalTime}")
  }
}
