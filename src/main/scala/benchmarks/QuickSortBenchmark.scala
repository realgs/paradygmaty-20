package benchmarks

import algorithms.QuickSort
import benchmarks.Benchmark.calculateExecutionTime
import scala.util.Random

/*
   Ratio = Sequential time / Parallel time

   Example results:

   Array Size: 1000
   Sequential time: 4 ms
   Parallel time: 5 ms
   Ratio: 0,8

   Array Size: 10_000
   Sequential time: 7 ms
   Parallel time: 13 ms
   Ratio: 0,53

   Array Size: 100_000
   Sequential time: 28 ms
   Parallel time: 34 ms
   Ratio: 0,82

   Array Size: 500_000
   Sequential time: 75 ms
   Parallel time: 132 ms
   Ratio: 0,56

   Array Size: 1_000_000
   Sequential time: 151 ms
   Parallel time: 176 ms
   Ratio: 0,85

   Array Size: 2_500_000
   Sequential time: 327 ms
   Parallel time: 203 ms
   Ratio: 1,61

   Array Size: 10_000_000
   Sequential time: 1102 ms
   Parallel time: 386 ms
   Ratio: 2,85

   Array Size: 100_000_000
   Sequential time: 10282 ms
   Parallel time: 2935 ms
   Ratio: 3,5

   Parallelized version of QuickSort, in my computer, starts giving better results than sequential one
   when array has about 2,5 million elements. In order to make computations as efficient as possible I'm
   checking if data is large enough and only then I use parallel version of this algorithm. Threshold helps
   me to avoid parallelism overhead
*/

object QuickSortBenchmark {
  private val RANDOM_ARRAY_TEST_SIZE = 10_000_000
  private val RANDOM_VALUE_BOUND = 100_000

  def run() {
    val sequentialArray = Array.fill(RANDOM_ARRAY_TEST_SIZE)(Random.nextInt(RANDOM_VALUE_BOUND))
    val parallelArray = sequentialArray.clone()

    calculateExecutionTime("QuickSortSequential") {
      QuickSort.sortSequential(sequentialArray)
    }

    calculateExecutionTime("QuickSortParallel") {
      QuickSort.sortParallel(parallelArray)
    }
  }
}
