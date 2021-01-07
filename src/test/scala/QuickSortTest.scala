import algorithms.QuickSort
import org.scalatest.FunSuite

import scala.util.Random

class QuickSortTest extends FunSuite{

  private val RANDOM_ARRAY_TEST_SIZE = 10_000_000

  test("SequentialQuickSortTest") {
    val array = Array.fill(RANDOM_ARRAY_TEST_SIZE)(Random.nextInt(100000))
    QuickSort.sortSequential(array)
    assert(isSorted(array))
  }

  test("ParallelQuickSortTest") {
    val array = Array.fill(RANDOM_ARRAY_TEST_SIZE)(Random.nextInt(100000))
    QuickSort.sortParallel(array)
    assert(isSorted(array))
  }

  test("BothQuickSortTest") {
    val sequentialArray = Array.fill(RANDOM_ARRAY_TEST_SIZE)(Random.nextInt(100000))
    val parallelArray = sequentialArray.clone()

    QuickSort.sortSequential(sequentialArray)
    QuickSort.sortParallel(parallelArray)

    assert(isSorted(sequentialArray))
    assert(isSorted(parallelArray))
    assert(sequentialArray.sameElements(parallelArray))
  }

  def isSorted[T <% Ordered[T]](array: Array[T]):Boolean ={
    (1 until array.length).forall(i => array(i-1) <= array(i))
  }
}
