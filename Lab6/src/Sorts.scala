import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}


object Sorts {

  def swap[A](tab: Array[A], i: Int, j: Int): Unit = {
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
        swap(tab, i, j);
        i += 1;
        j -= 1
      }
    }
    (i, j)
  }

  def quick(tab: Array[Int], l: Int, r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if ((j - l) < (r - i)) {
        quick(tab, l, j)
        quick(tab, i, r)
      } else {
        quick(tab, i, r)
        quick(tab, l, j)
      }
    }
  }


  def quickParallel(tab: Array[Int], l: Int, r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if ((j - l) < (r - i)) {
        parallel(quick(tab, l, j), quickFuture(tab, i, r))
      } else {
        parallel(quick(tab, i, r), quickFuture(tab, l, j))
      }
    }
  }

  def quickFuture(tab: Array[Int], l: Int, r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if (j - l < r - i) {
        val smallerTab = Future(quick(tab, l, j))
        val biggerTab = Future(quick(tab, i, r))
        Await.result(smallerTab, 2000.seconds)
        Await.result(biggerTab, 2000.seconds)
      } else {
        val smallerTab = Future(quick(tab, i, r))
        val biggerTab = Future(quick(tab, l, j))
        Await.result(smallerTab, 2000.seconds)
        Await.result(biggerTab, 2000.seconds)
      }
    }
  }

  def quicksort(tab: Array[Int]): Unit = quick(tab, 0, tab.length - 1)

  def quicksortFuture(tab: Array[Int]): Unit = quickFuture(tab, 0, tab.length - 1)

  def quicksortParallel(tab: Array[Int]): Unit = quickParallel(tab, 0, tab.length - 1)


  def time[A](block: => A): Unit = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    println("Total time: " + (t1 - t0) + "ms")
  }
}
