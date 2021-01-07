package algorithms

import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

object QuickSort {
  private val PARALLEL_SORT_THRESHOLD = 500_000

  def sortSequential(array: Array[Int]): Unit = quickSequential(array)(0)(array.length - 1)

  def sortParallel(array: Array[Int]): Unit = {
    if (array.length >= PARALLEL_SORT_THRESHOLD) {
      val pool = new ForkJoinPool(Runtime.getRuntime.availableProcessors())
      pool.invoke(new QuickSortParallel(array, 0, array.length - 1))
    } else {
      sortSequential(array)
    }
  }

  private def swap[A](array: Array[A])(i: Int)(j: Int): Unit = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

  private def choosePivot[A](array: Array[A])(m: Int)(n: Int): A = array((m + n) / 2)

  def partition(array: Array[Int])(l: Int)(r: Int): (Int, Int) = {
    var i = l
    var j = r
    val pivot = choosePivot(array)(l)(r)

    while (i <= j) {
      while (array(i) < pivot) i = i + 1
      while (array(j) > pivot) j = j - 1

      if (i <= j) {
        swap(array)(i)(j)
        i += 1
        j -= 1
      }
    }

    (i, j)
  }

  private def quickSequential(array: Array[Int])(l: Int)(r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition(array)(l)(r)

      if (j - 1 < r - 1) {
        quickSequential(array)(l)(j)
        quickSequential(array)(i)(r)
      }
      else {
        quickSequential(array)(i)(r)
        quickSequential(array)(l)(j)
      }
    }
  }

  class QuickSortParallel(array: Array[Int], left: Int, right: Int) extends RecursiveTask[Unit] {
    override def compute(): Unit = {
      if (left < right) {
        val (i, j) = partition(array)(left)(right)

        if (j - 1 < right - 1) {
          val leftArraySortOperation = new QuickSortParallel(array, left, j)
          val rightArraySortOperation = new QuickSortParallel(array, i, right)

          invokeAll(leftArraySortOperation, rightArraySortOperation)
        }
        else {
          val leftArraySortOperation = new QuickSortParallel(array, i, right)
          val rightArraySortOperation = new QuickSortParallel(array, left, j)

          invokeAll(leftArraySortOperation, rightArraySortOperation)
        }
      }
    }
  }

}
