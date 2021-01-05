import Parallel._

import scala.language.postfixOps

object QuickSort {
  var THRESHOLD = 4

  private def sort(array: Array[Int]): Array[Int] = {
    if (array.length <= 1) array
    else {
      val pivot = array(array.length / 2)
      Array.concat(
        sort(array filter (pivot >)),
        array filter (pivot ==),
        sort(array filter (pivot <))
      )
    }
  }

  private def parallelSort(array: Array[Int], threshold: Int): Array[Int] = {
    if (array.length <= 1) array
    else {
      if (threshold > 0) {
        val pivot = array(array.length / 2)
        val (left, right) = parallel(parallelSort(array filter (pivot >), threshold - 1), parallelSort(array filter (pivot <), threshold - 1))
        Array.concat(
          left, array filter (pivot ==), right
        )
      }
      else sort(array)
    }
  }

  def quickSort(array: Array[Int]): Array[Int] = sort(array)

  def quickSortParallel(array: Array[Int]): Array[Int] = parallelSort(array, THRESHOLD)
}
