package Algorithms

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object QuickSort {
  private def swap[A](array: Array[A], i: Int, j: Int): Unit = {
    val aux = array(i)
    array(i) = array(j)
    array(j) = aux
  }

  private def partition(array: Array[Int], left: Int, right: Int): (Int, Int) = {
    var i = left
    var j = right
    val pivot = array((i + j) / 2)
    while (i <= j) {
      while (array(i) < pivot) i += 1
      while (array(j) > pivot) j -= 1
      if (i <= j) {
        swap(array, i, j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  private def quick(array: Array[Int], left: Int, right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(array, left, right)
      if (j - left < right - i) {
        quick(array, left, j)
        quick(array, i, right)
      }
      else {
        quick(array, i, right)
        quick(array, left, j)
      }
    }
  }

  private def quickParallel(array: Array[Int], left: Int, right: Int, depth: Int): Unit = {
    if (depth == 0)
      quick(array, left, right)
    if (left < right) {
      val (i, j) = partition(array, left, right)
      if (j - left < right - i) {
        val futureLeft = Future(quickParallel(array, left, j, depth - 1))
        val futureRight = Future(quickParallel(array, i, right, depth - 1))
        Await.result(futureLeft, 1000.seconds)
        Await.result(futureRight, 1000.seconds)
      }
      else {
        val futureRight = Future(quick(array, i, right))
        val futureLeft = Future(quick(array, left, j))
        Await.result(futureLeft, 1000.seconds)
        Await.result(futureRight, 1000.seconds)
      }
    }
  }


  def quickSort(array: Array[Int]): Unit = {
    quick(array, 0, array.length - 1)
  }

  def quickSortParallel(array: Array[Int]): Unit = {
    quickParallel(array, 0, array.length - 1, Runtime.getRuntime.availableProcessors()/2 - 1)
  }
}
