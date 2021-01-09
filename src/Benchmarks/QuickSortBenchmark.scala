// Konrad Karanowski
package Benchmarks

import org.scalameter._
import Quicksort.{quicksortSequential, quicksortParallel}
import scala.util.Random

object QuickSortBenchmark
{

  private def testAlgorithm(quicksort:  Array[Int] => Unit)(
    array: Array[Int],
    runsPerArray: Int,
    verbose: Boolean = false
  ): Double =
  {
    val time = config(
      Key.exec.maxWarmupRuns -> runsPerArray,
      Key.verbose -> verbose,
    ) withWarmer(new Warmer.Default) measure
    {
      array.clone()
      quicksort(array)
    }
    time.value
  }

  private def testRun(arraySize: Int, runsPerArray: Int): (Double, Double)=
  {
    val array = Array.fill(arraySize)(Random.nextInt(1000))
    (
      testAlgorithm(quicksortSequential)(array, runsPerArray),
      testAlgorithm(quicksortParallel)(array, runsPerArray)
    )
  }

  def testQuicksort(differentRuns: Int, arraySize: Int, runsPerArray: Int): Unit =
  {
    var parallelTotalTime = .0
    var sequentialTotalTime = .0
    for (_ <- 0 until differentRuns)
    {
      val (sequentialTime, parallelTime) = testRun(arraySize, runsPerArray)
      parallelTotalTime += parallelTime
      sequentialTotalTime += sequentialTime
    }
    println(f"Different runs: $differentRuns, Size of an array: $arraySize, Runs per array: $runsPerArray")
    println(f"Mean time for the sequential solution: ${sequentialTotalTime / differentRuns}ms")
    println(f"Mean time for the parallel solution: ${parallelTotalTime / differentRuns}ms")
    println(f"Ratio sequential/parallel: ${sequentialTotalTime / parallelTotalTime}")
  }
}
