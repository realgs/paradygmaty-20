import org.scalatest.flatspec.AnyFlatSpec
import QuickSort._

class QuickSortTest extends AnyFlatSpec {
  "QuickSort method" should "sort properly" in {
    assert((quickSort(Array(1, 2, 1, -3, 6, 1, 8, 53, 123, 1004, -97)): Seq[Int]) == (Array(-97, -3, 1, 1, 1, 2, 6, 8, 53, 123, 1004): Seq[Int]))
    assert((quickSort(Array(1, 0, 3, -1000)): Seq[Int]) == (Array(-1000, 0, 1, 3): Seq[Int]))
    assert((quickSort(Array(1, 2, 5)): Seq[Int]) == (Array(1, 2, 5): Seq[Int]))
  }

  "QuickSort method" should "work properly for empty arrays" in {
    assert((quickSort(Array()): Seq[Int]) == (Array[Int](): Seq[Int]))
  }

  "QuickSort parallel method" should "sort properly" in {
    assert((quickSortParallel(Array(1, 2, 1, -3, 6, 1, 8, 53, 123, 1004, -97)): Seq[Int]) == (Array(-97, -3, 1, 1, 1, 2, 6, 8, 53, 123, 1004): Seq[Int]))
    assert((quickSortParallel(Array(1, 0, 3, -1000)): Seq[Int]) == (Array(-1000, 0, 1, 3): Seq[Int]))
    assert((quickSortParallel(Array(1, 2, 5)): Seq[Int]) == (Array(1, 2, 5): Seq[Int]))
  }

  "QuickSort parallel method" should "work properly for empty arrays" in {
    assert((quickSortParallel(Array()): Seq[Int]) == (Array[Int](): Seq[Int]))
  }
}
