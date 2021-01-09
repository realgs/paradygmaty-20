import org.scalatest.FunSuite

class MergeSortTest extends FunSuite{

  test("mergeSort_test_1"){
    val array = Array(23,7,45,2,5,9,54,86,1,3,45)
    MergeSort.mergeSort(array)
    assert(array === Array(1,2,3,5,7,9,23,45,45,54,86))
  }
  test("mergeSort_test_2"){
    val array = Array(7,4,2,5,9,5,8,1,3,0)
    MergeSort.mergeSort(array)
    assert(array === Array(0,1,2,3,4,5,5,7,8,9))
  }
  test("parallelMergeSort_test_1"){
    val array = Array(23,7,45,2,5,9,54,86,1,3,45)
    MergeSort.parallelMergeSort(array)
    assert(array === Array(1,2,3,5,7,9,23,45,45,54,86))
  }
  test("parallelMergeSort_test_2"){
    val array = Array(7,4,2,5,9,5,8,1,3,0)
    MergeSort.parallelMergeSort(array)
    assert(array === Array(0,1,2,3,4,5,5,7,8,9))
  }
}
