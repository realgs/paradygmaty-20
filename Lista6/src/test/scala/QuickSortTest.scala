import org.scalatest.FunSuite
import QuickSort._
import Utilities.random

class QuickSortTest extends FunSuite {

  test("QuickSort - quickSort corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) //negative numbers included
      val referenceArray = initialArray.clone()
      quickSort(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }

  test("QuickSort - quickSortFuture corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) ////negative numbers included
      val referenceArray = initialArray.clone()
      quickSortFuture(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }

  test("QuickSort - quickSortParallel corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) ////negative numbers included
      val referenceArray = initialArray.clone()
      quickSortParallel(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }
}
