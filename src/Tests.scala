// Konrad Karanowski
import scala.math.{pow, sin}
import Benchmarks._

/*
  I used 3 different methods to parallelize computations:

  a) Parallelize recursive algorithms using Futures:
    We specify max depth. Before this we use Futures from scala.concurrency and wait for calculations to be done.
    I used this method to optimize Quicksort (The imperative version of the task which consisted in
    rewriting the lecturer's code to the Scala).

  b) Parallelize numeric computations using parallel collections:
    We specify the number of tasks into which we divide the problem. Every step is executed in parallel.
    The number of tasks can be equal to all the number of possible cores.
    I used this method to optimize calculating define integrals using Monte Carlo method (functional paradigm).
    Every task returns value and then, they are summarized.

  c) Parallelize matrix algorithms by running for-loop in parallel:
    We specify the number of tasks into which we divide the problem. Every step is executed in parallel.
    The number of tasks can be equal to all the number of possible cores.
    I used this method to optimize: matrix multiplication (imperative version) and Floyd-Warshall algorithm (also imperative).
    This approach can potentially reduce complexity from O(n^3) to O(n) (of course it is almost impossible).
    Other methods I tried were: parallelize whole loop (much time is wasted on excessive parallelization),
    divide matrices for areas and perform operations on them.

  To benchmark I used the Scalameter library. Each test was run 100 times (with warmer) for 10 different inputs
  of a certain size. Benchmarks are not perfect, as I measured only mean time of 1000 runs for certain input size, there
  can also occur garbage collector and distort the measurements.

  Results:

  a) quicksort:

     1) Size: 10
        Mean sequential time: 0.0056ms
        Mean parallel time: 0.0908ms
        Ratio parallel / sequential: 0.0610

     2) Size: 100
        Mean sequential time: 0.0058ms
        Mean parallel time: 0.14ms
        Ratio parallel / sequential: 0.040

     3) Size: 1000
        Mean sequential time: 0.032ms
        Mean parallel time: 0.140ms
        Ratio parallel / sequential: 0.210

     4) Size: 10_000
        Mean sequential time: 0.46ms
        Mean parallel time: 0.37ms
        Ratio parallel / sequential: 1.22

     5) Size: 100_000
        Mean sequential time: 3.5ms
        Mean parallel time: 1.61ms
        Ratio parallel / sequential: 2.16

     6) Size: 1_000_000
        Mean sequential time: 45.94ms
        Mean parallel time: 18.28ms
        Ratio parallel / sequential: 2.51

  b) monte carlo:

     1) Size: 10
        Mean sequential time: 0.0056ms
        Mean parallel time: 0.252ms
        Ratio parallel / sequential: 0.0220

     2) Size: 100
        Mean sequential time: 0.0053ms
        Mean parallel time: 0.154ms
        Ratio parallel / sequential: 0.0340

     3) Size: 1000
        Mean sequential time: 0.029ms
        Mean parallel time: 0.15ms
        Ratio parallel / sequential: 0.28

     4) Size: 10_000
        Mean sequential time: 0.29ms
        Mean parallel time: 0.22ms
        Ratio parallel / sequential: 1.30

     5) Size: 100_000
        Mean sequential time: 2.7ms
        Mean parallel time: 0.84ms
        Ratio parallel / sequential: 3.20

     6) Size: 1_000_000
        Mean sequential time: 25.76ms
        Mean parallel time: 7.45ms
        Ratio parallel / sequential: 3.46

  c) matrix multiplication:

     1) Size: 10
        Mean sequential time: 0.22ms
        Mean parallel time: 2.45ms
        Ratio parallel / sequential: 0.09

     2) Size: 100
        Mean sequential time: 2.16ms
        Mean parallel time: 1.59ms
        Ratio parallel / sequential: 1.45

     3) Size: 1_000
        Mean sequential time: 1574.11ms
        Mean parallel time: 348.07ms
        Ratio parallel / sequential: 4.52

   d) floyd-warshall algorithm:

        1) Size: 10
        Mean sequential time: 0.31ms
        Mean parallel time: 2.45ms
        Ratio parallel / sequential: 0.10

     2) Size: 100
        Mean sequential time: 2.55ms
        Mean parallel time: 1.4ms
        Ratio parallel / sequential: 1.83

     3) Size: 1_000
        Mean sequential time: 3641.53ms
        Mean parallel time: 846.87ms
        Ratio parallel / sequential: 4.30ms

    Conclusions:
      Parallelization makes our algorithms way faster if the data is large enough.
      Obviously not every algorithm are can be paralleled, and some of them must be sequential.
      For this reasons, parallelization is great for example for big data applications
      and machine learning algorithms (such as ensemble ones).
 */

object Tests
{

  private def printBreakingLine(): Unit =
  {
    for(_ <- 1 to 80)
    {
      print('=')
    }
    print('\n')
  }

  private def quicksortTests(): Unit =
  {
    println("QUICKSORT TESTS")
    for (i <- 1 to 6)
    {
      printBreakingLine()
      val size = pow(10, i).toInt
      QuickSortBenchmark.testQuicksort(10, size, 100)
    }
  }

  private def matrixMultiplicationTests(): Unit =
  {
    println("MATRIX MULTIPLICATION TESTS")
    for (i <- 1 to 3)
    {
      printBreakingLine()
      val size = pow(10, i).toInt
      MatrixMultiplicationBenchmark.testMatrixMultiplication(10, (size, size), (size, size), 100)
    }
  }

  private def allPairsShortestPathsTests(): Unit =
  {
    println("ALL PAIRS SHORTEST PATHS TESTS")
    for (i <- 1 to 3)
    {
      printBreakingLine()
      val size = pow(10, i).toInt
      AllPairsShortestPathsBenchmark.testAllShortestPaths(10, size, 100)
    }
  }

  private def monteCarloIntegrationTests(): Unit =
  {
    println("MONTE CARLO INTEGRATION")
    for (i <- 1 to 6)
    {
      printBreakingLine()
      val size = pow(10, i).toInt
      MonteCarloBenchmarks.testMonteCarlo(10, sin, .0, 3.14, size, 100)
    }
  }

  def main(args: Array[String]): Unit =
  {
    quicksortTests()
    monteCarloIntegrationTests()
    matrixMultiplicationTests()
    allPairsShortestPathsTests()
  }
}
