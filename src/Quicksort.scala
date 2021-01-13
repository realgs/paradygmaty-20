import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

object Quicksort {
  def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) = {
    var i = l
    var j = r
    val pivot = tab((l + r) / 2)

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

  def quick(tab: Array[Int], l: Int, r: Int): Unit =
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if (j - l < r - i) {
        quick(tab, l, j)
        quick(tab, i, r)
      }
      else {
        quick(tab, i, r)
        quick(tab, l, j)
      }
    }

  def quickParallel(tab: Array[Int], l: Int, r: Int): Unit =
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if (j - l < r - i)
        parallel(quick(tab, l, j), quick(tab, i, r))
      else
        parallel(quick(tab, i, r), quick(tab, l, j))
    }

  def quickFuture(tab: Array[Int], l: Int, r: Int): Unit =
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if (j - l < r - i) {
        Await.result(Future(quick(tab, l, j)), 2000.second)
        Await.result(Future(quick(tab, i, r)), 2000.second)
      }
      else {
        Await.result(Future(quick(tab, i, r)), 2000.second)
        Await.result(Future(quick(tab, l, j)), 2000.second)
      }
    }

  def quicksort(sortingFunction: (Array[Int], Int, Int) => Unit, tab: Array[Int]): Unit = sortingFunction(tab, 0, tab.length - 1)
}

