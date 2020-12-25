package QuickSort
import ParallelMachine.ParallelMachine.parallel
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object QuickSort {
  //basic version of this algorithm was made for my Programming Paradigms classes

  private def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
    val temp = tab(i)
    tab(i) = tab(j)
    tab(j) = temp
  }

  private def choosePivot[A](tab: Array[A])(m: Int)(n: Int): A =
    tab((m + n) / 2)

  private def partition(tab: Array[Int])(left: Int)(right: Int): (Int, Int) = {
    var i = left
    var j = right
    val pivot = choosePivot(tab)(left)(right)
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

  private def quick(tab: Array[Int])(left: Int)(right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(tab)(left)(right)
      if (j - left < right - i) {
        quick(tab)(left)(j)
        quick(tab)(i)(right)
      }
      else {
        quick(tab)(i)(right)
        quick(tab)(left)(j)
      }
    }
  }

  private def quickFuture(tab: Array[Int])(left: Int)(right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(tab)(left)(right)
      if (j - left < right - i) {
        val fut1 = Future(quick(tab)(left)(j))
        val fut2 = Future(quick(tab)(i)(right))
        Await.result(fut1, 100.seconds) // waits at most 100 seconds for the result
        Await.result(fut2, 100.seconds) // safer than Duration.Inf
      }
      else {
        val fut1 = Future(quick(tab)(i)(right))
        val fut2 = Future(quick(tab)(left)(j))
        Await.result(fut1, 100.seconds)
        Await.result(fut2, 100.seconds)
      }
    }
  }

  private def quickParallel(tab: Array[Int])(left: Int)(right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(tab)(left)(right)
      if (j - left < right - i)
        parallel(quick(tab)(left)(j), quick(tab)(i)(right))
      else
        parallel(quick(tab)(i)(right), quick(tab)(left)(j))
    }
  }

  def quickSort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length - 1)

  def quickSortFuture(tab: Array[Int]): Unit =
    quickFuture(tab)(0)(tab.length - 1)

  def quickSortParallel(tab: Array[Int]): Unit =
    quickParallel(tab)(0)(tab.length - 1)

}
