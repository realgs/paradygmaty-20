import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object Quicksort {

  def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  def choose_pivot[A](tab: Array[A])(m: Int)(n: Int): A = tab((m+n)/2)

  def partition (tab: Array[Int])(l: Int)(r: Int): (Int, Int) = {
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)
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

  def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
    if (l < r) {
      val (i, j) = partition(tab)(l)(r)
      val _ = quick(tab)(l)(j)
      quick(tab)(i)(r)
    }

  def quicksort(tab: Array[Int]): Unit = quick(tab)(0)(tab.length - 1)

  def parallelQuick (tab: Array[Int])(l: Int)(r: Int)(depth: Int): Unit = {
    if (depth == 0)
      quick(tab)(l)(r)
    else {
      if (l < r) {
        val (i, j) = partition(tab)(l)(r)
        val futureL = Future(parallelQuick(tab)(l)(j)(depth - 1))
        val futureR = Future(parallelQuick(tab)(i)(r)(depth - 1))

        Await.result(futureL, 10.minutes)
        Await.result(futureR, 10.minutes)
      }
    }
  }

  def quicksortParallel(tab: Array[Int]): Unit = parallelQuick(tab)(0)(tab.length - 1)(6)

}
