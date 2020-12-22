import ParallelMechanism._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Success, Failure}

object MergeSort {
  //in-place merge sort - reduces amount of used memory

  private def swap[A](tab: Array[A], i: Int, j: Int): Unit = {
    val temp = tab(i)
    tab(i) = tab(j)
    tab(j) = temp
  }

  private def putInCorrectPlace(tab: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    for(i <- startIndex until endIndex) {
      if(tab(i) > tab(i + 1))
        swap(tab, i, i+1)
    }
  }

  private def merge(tab: Array[Int], startIndex: Int, endIndex: Int, midIndex: Int): Unit = {
    var iter = startIndex
    while(iter <= midIndex) {
      if(tab(iter) > tab(midIndex + 1)) {
        swap(tab, iter, midIndex + 1)
        putInCorrectPlace(tab, midIndex + 1, endIndex)
      }
      iter += 1
    }
  }

  private def mergeSort(tab: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    if(endIndex - startIndex != 0) { // checks if array contains more than 1 element
      if(endIndex - startIndex == 1) {
        if(tab(startIndex) > tab(endIndex))
          swap(tab, startIndex, endIndex)
      }
      else {
        val midIndex = (endIndex + startIndex) / 2
        mergeSort(tab, startIndex, midIndex)
        mergeSort(tab, midIndex + 1, endIndex)
        merge(tab, startIndex, endIndex, midIndex)
      }
    }
  }

  private def futureMergeSort(tab: Array[Int], startIndex: Int = 0, endIndex: Int): Unit = {
    if(endIndex - startIndex != 0) { // checks if array contains more than 1 element
      if(endIndex - startIndex == 1) {
        if(tab(startIndex) > tab(endIndex))
          swap(tab, startIndex, endIndex)
      }
      else {
        val midIndex = (endIndex + startIndex) / 2
        val fut1 = Future(mergeSort(tab, startIndex, midIndex))
        val fut2 = Future(mergeSort(tab, midIndex + 1, endIndex))
        Await.result(fut1, 1000.seconds)                                  // waits at most 1000 seconds for the result
        Await.result(fut2, 1000.seconds)                                  // sefer than Duration.Inf
        merge(tab, startIndex, endIndex, midIndex)
      }
    }
  }

  private def parallelMergeSort(tab: Array[Int], startIndex: Int = 0, endIndex: Int): Unit = {
    if(endIndex - startIndex != 0) { // checks if array contains more than 1 element
      if(endIndex - startIndex == 1) {
        if(tab(startIndex) > tab(endIndex))
          swap(tab, startIndex, endIndex)
      }
      else {
        val midIndex = (endIndex + startIndex) / 2
        parallel(mergeSort(tab, startIndex, midIndex), mergeSort(tab, midIndex + 1, endIndex))
        merge(tab, startIndex, endIndex, midIndex)
      }
    }
  }

  def mergeSort(tab: Array[Int]): Unit =
    mergeSort(tab, 0, tab.length - 1)

  def mergeSortFuture(tab: Array[Int]): Unit =
    futureMergeSort(tab, 0, tab.length - 1)

  def mergeSortParallel(tab: Array[Int]): Unit =
    parallelMergeSort(tab, 0, tab.length - 1)

}
