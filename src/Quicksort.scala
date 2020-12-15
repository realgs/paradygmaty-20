import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
        Future(quick(tab, l, j))
        Future(quick(tab, i, r))
      }
      else {
        Future(quick(tab, i, r))
        Future(quick(tab, l, j))
      }
    }

  def quicksort(sortingFunction: (Array[Int], Int, Int) => Unit, tab: Array[Int]): Unit = sortingFunction(tab, 0, tab.length - 1)
}

