package benchmarks

import algorithms.QuickSort
import benchmarks.Benchmark.calculateExecutionTime
import scala.util.Random

/*
   Ratio = Sequential time / Parallel time

   Example results:

   WITHOUT THRESHOLD

   Array Size: 100
   Sequential time: 2.21 ms
   Parallel time: 3.31 ms
   Ratio: 0,67

   Array Size: 1000
   Sequential time: 3,42 ms
   Parallel time: 5,44 ms
   Ratio: 0,62

   WITH THRESHOLD

   Array Size: 10_000
   Sequential time: 5.61 ms
   Parallel time: 3.95 ms
   Ratio: 1,42

   Array Size: 100_000
   Sequential time: 27.96 ms
   Parallel time: 9.48 ms
   Ratio: 2,94

   Array Size: 1_000_000
   Sequential time: 122.71 ms
   Parallel time: 36.36 ms
   Ratio: 3,37

   Array Size: 10_000_000
   Sequential time: 945,61 ms
   Parallel time: 225.51 ms
   Ratio: 4,19

   Array Size: 100_000_000
   Sequential time: 10560.65 ms
   Parallel time: 2532.39 ms
   Ratio: 4,17

   The more elements the array has, the better the time performance of the parallel version of QuickSort.
   When arrays were small (less than 1k elements) sequential version got better results than the parallel one.
   To avoid this problem I used threshold in my implementation i.e. when the array size is smaller than 1000
   I perform sequential computations, otherwise, I divide my problem into smaller parts
*/

object QuickSortBenchmark {
  private val RANDOM_ARRAY_TEST_SIZE = 1_000_000
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
