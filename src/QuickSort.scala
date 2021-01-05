import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object QuickSort {
  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  def partition(arr: Array[Int], low: Int, high: Int): Int = {
    val pivot = Math.random.toInt * (high - low) + arr(high)
    var i = low - 1
    for (j <- low until high) {
      if (arr(j) < pivot) {
        i += 1
        swap(arr, i, j)
      }
    }
    swap(arr, i + 1, high)
    i + 1
  }

  def quickSimple(arr: Array[Int], low: Int, high: Int): Unit = {
    if (low < high) {
      val pi = partition(arr, low, high)
      quickSimple(arr, low, pi - 1)
      quickSimple(arr, pi + 1, high)
    }
  }

  def quickParallel(arr: Array[Int], low: Int, hight: Int): Unit = {
    if (low < hight) {
      val pi = partition(arr, low, hight)
      Future(quickSimple(arr, low, pi - 1))
      Future(quickSimple(arr, pi + 1, hight))
    }
  }

  def quickSortSimple(array: Array[Int]): Unit =
    quickSimple(array, 0, array.length - 1)

  def quickSortParallel(tab: Array[Int]): Unit =
    quickParallel(tab, 0, tab.length - 1)
}
