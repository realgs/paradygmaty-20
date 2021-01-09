import Benchmark.calculateExecutionTime
import org.scalatest.FunSuite

import scala.util.Random

/**
 * Benchmark
 *
 * Test 100_000_000
 * QuickSort.sequential() took 15276ms
 * QuickSort.parallel() took 4665ms
 *
 * Test 10_000_000
 * QuickSort.sequential() took 1316ms
 * QuickSort.parallel() took 342ms
 *
 * Test 1_000_000
 * QuickSort.sequential() took 181ms
 * QuickSort.parallel() took 36ms
 *
 * Test 100_000
 * QuickSort.sequential() took 37ms
 * QuickSort.parallel() took 25ms
 */
class QuickSortTest extends FunSuite {
  private val ARRAY_TEST_SIZE = 10_000_000
  private val RANDOM_VALUE_MAX = 100_000

  test("QuickSortSequentialTest") {
    val array = Array.fill(ARRAY_TEST_SIZE)(Random.nextInt(RANDOM_VALUE_MAX))
    QuickSort.sequential(array)
    assert(isSorted(array))
  }

  test("QuickSortParallelTest") {
    val array = Array.fill(ARRAY_TEST_SIZE)(Random.nextInt(RANDOM_VALUE_MAX))
    QuickSort.parallel(array)
    assert(isSorted(array))
  }

  test("QuickSortBenchmarkTest") {
    val array = Array.fill(ARRAY_TEST_SIZE) {
      Random.nextInt(RANDOM_VALUE_MAX)
    }
    val arrayCopy = array.clone()

    calculateExecutionTime("QuickSort.sequential()") {
      QuickSort.sequential(array)
    }
    calculateExecutionTime("QuickSort.parallel()") {
      QuickSort.parallel(arrayCopy)
    }
  }

  def isSorted(array: Array[Int]): Boolean = (1 until array.length).forall(i => array(i - 1) <= array(i))
}
