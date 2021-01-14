import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Quicksort {
  private def swap[A](tab: Array[A], i: Int, j: Int): Unit = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  private def choosePivot[A](tab: Array[A], startIndex: Int, endIndex: Int): A =
    tab((startIndex + endIndex) / 2)

  private def partition(tab: Array[Int], startIndex: Int, endIndex: Int): (Int, Int) = {
    var i = startIndex
    var j = endIndex
    val pivot = choosePivot(tab, startIndex, endIndex)
    while(i <= j) {
      while(tab(i) < pivot) i += 1
      while(pivot < tab(j)) j -= 1
      if(i <= j) {
        swap(tab, i, j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  private def quick(tab: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    if(startIndex < endIndex) {
      val (i, j) = partition(tab, startIndex, endIndex)
      if(j - startIndex < endIndex - i) {
        quick(tab, startIndex, j)
        quick(tab, i, endIndex)
      }
      else {
        quick(tab, i, endIndex)
        quick(tab, startIndex, j)
      }
    }
  }

  def quicksort(tab: Array[Int]): Unit =
    quick(tab, 0 , tab.length - 1)

  private def quickParallel(tab: Array[Int], startIndex: Int, endIndex: Int, depth: Int): Unit = {
    if(depth > 0) {
      if (startIndex < endIndex) {
        val (i, j) = partition(tab, startIndex, endIndex)
        val future = Future(quickParallel(tab, startIndex, j, depth - 1))
        quick(tab, i, endIndex)
        Await.result(future, 10.minutes)
      }
    } else quick(tab, startIndex, endIndex)
  }

  def quicksortParallel(depth: Int)(tab: Array[Int]): Unit =
    quickParallel(tab, 0, tab.length - 1, depth)
}
