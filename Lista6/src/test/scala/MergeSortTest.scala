import Utilities.random
import org.scalatest.FunSuite
import MergeSort._

class MergeSortTest extends FunSuite{

  test("MergeSort - mergeSort corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) //negative numbers included
      val referenceArray = initialArray.clone()
      mergeSort(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }

  test("MergeSort - mergeSortFuture corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) //negative numbers included
      val referenceArray = initialArray.clone()
      mergeSortFuture(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }

  test("MergeSort - mergeSortParallel corectness test") {
    for(_ <- 0 until 10) { // for example 10 tests
      val initialArray = Array.fill(1000)(random.nextInt(10000)-500) //negative numbers included
      val referenceArray = initialArray.clone()
      mergeSortParallel(initialArray)
      assert(referenceArray.sortWith(_<_) sameElements initialArray)
    }
  }


}
