import Parallel._

object MergeSort {
  val THRESHOLD = 4

  private def merge(array: Array[Int], leftArray: Array[Int], rightArray: Array[Int], leftSize: Int, rightSize: Int): Unit = {
    var i = 0
    var j = 0
    var k = 0

    while (i < leftSize && j < rightSize) {
      if (leftArray(i) <= rightArray(j)) {
        array(k) = leftArray(i)
        k += 1
        i += 1
      }
      else {
        array(k) = rightArray(j)
        k += 1
        j += 1
      }
    }

    while (i < leftSize) {
      array(k) = leftArray(i)
      k += 1
      i += 1
    }

    while (j < rightSize) {
      array(k) = rightArray(j)
      k += 1
      j += 1
    }
  }

  private def mergeSort(array: Array[Int], size: Int): Unit = {
    if (size < 2) return
    val mid = size / 2
    val leftArray = new Array[Int](mid)
    val rightArray = new Array[Int](size - mid)

    for (i <- Range(0, mid)) leftArray(i) = array(i)
    for (i <- Range(0, size - mid)) rightArray(i) = array(mid + i)

    mergeSort(leftArray, mid)
    mergeSort(rightArray, size - mid)

    merge(array, leftArray, rightArray, mid, size - mid)
  }

  private def parMergeSort(array: Array[Int], size: Int, threshold: Int): Unit = {
    if (size < 2) return
    val mid = size / 2
    val leftArray = new Array[Int](mid)
    val rightArray = new Array[Int](size - mid)

    for (i <- Range(0, mid)) leftArray(i) = array(i)
    for (i <- Range(0, size - mid)) rightArray(i) = array(mid + i)

    if (threshold > 0) parallel(parMergeSort(leftArray, mid, threshold - 1), parMergeSort(rightArray, size - mid, threshold - 1))
    else {
      mergeSort(leftArray, mid)
      mergeSort(rightArray, size - mid)
    }

    merge(array, leftArray, rightArray, mid, size - mid)
  }

  def sortWithMergeSort(array: Array[Int]): Unit = mergeSort(array, array.length)

  def sortWithMergeSortParallel(array: Array[Int]): Unit = parMergeSort(array, array.length, THRESHOLD)
}
