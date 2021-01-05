import org.scalatest.flatspec.AnyFlatSpec
import MergeSort._

class MergeSortTest extends AnyFlatSpec {
  "MergeSort method" should "sort properly" in {
    val firstArray = Array(1, 2, 1, -3, 6, 1, 8, 53, 123, 1004, -97)
    val secondArray = Array(1, 0, 3, -1000)
    val thirdArray = Array(1, 2, 5)

    sortWithMergeSort(firstArray)
    sortWithMergeSort(secondArray)
    sortWithMergeSort(thirdArray)
    assert((firstArray: Seq[Int]) == (Array(-97, -3, 1, 1, 1, 2, 6, 8, 53, 123, 1004): Seq[Int]))
    assert((secondArray: Seq[Int]) == (Array(-1000, 0, 1, 3): Seq[Int]))
    assert((thirdArray: Seq[Int]) == (Array(1, 2, 5): Seq[Int]))
  }

  "MergeSort method" should "work properly for empty arrays" in {
    val emptyArray = Array[Int]()
    sortWithMergeSort(emptyArray)
    assert((emptyArray: Seq[Int]) == (Array[Int](): Seq[Int]))
  }

  "MergeSort parallel method" should "sort properly" in {
    val firstArray = Array(1, 2, 1, -3, 6, 1, 8, 53, 123, 1004, -97)
    val secondArray = Array(1, 0, 3, -1000)
    val thirdArray = Array(1, 2, 5)

    sortWithMergeSortParallel(firstArray)
    sortWithMergeSortParallel(secondArray)
    sortWithMergeSortParallel(thirdArray)
    assert((firstArray: Seq[Int]) == (Array(-97, -3, 1, 1, 1, 2, 6, 8, 53, 123, 1004): Seq[Int]))
    assert((secondArray: Seq[Int]) == (Array(-1000, 0, 1, 3): Seq[Int]))
    assert((thirdArray: Seq[Int]) == (Array(1, 2, 5): Seq[Int]))
  }

  "MergeSort parallel method" should "work properly for empty arrays" in {
    val emptyArray = Array[Int]()
    sortWithMergeSortParallel(emptyArray)
    assert((emptyArray: Seq[Int]) == (Array[Int](): Seq[Int]))
  }
}
