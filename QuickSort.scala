import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object QuickSort {

  private def partition(tab: Array[Int], start: Int, end: Int): (Int, Int) = {
    var i = start
    var j = end
    val pivot = tab((start + end) / 2)
    while (i <= j) {
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) {
        swap(tab)(i)(j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  private def swap[A](tab: Array[A])(left: Int)(right: Int): Unit = {
    val temp = tab(left)
    tab(left) = tab(right)
    tab(right) = temp
  }

  private def quick(tab: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val (i, j) = partition(tab, start, end)
      if (j - start < end - i) {
        quick(tab, start, j)
        quick(tab, i, end)
      }
      else {
        quick(tab, i, end)
        quick(tab, start, j)
      }
    }
  }

  private def quickParallel(tab: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val (i, j) = partition(tab, start, end)
      if (j - start < end - i) {
        val fut1 = Future{quick(tab, start, j)}
        val fut2 = Future{quick(tab, i, end)}

      }
      else {
        val fut1 = Future{quick(tab, i, end)}
        val fut2 = Future{quick(tab, start, j)}

      }
    }
  }

  def quickSort(tab: Array[Int]): Unit =
    quick(tab, 0, tab.length - 1)

  def quickSortParallel(tab: Array[Int]): Unit =
    quickParallel(tab, 0, tab.length - 1)
}
