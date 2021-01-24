import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random

object QuickSort {
  def sequential(array: Array[Int]): Unit = {
    sequentialSort(array, 0, array.length - 1)
  }

  private def sequentialSort(array: Array[Int], begin: Int, end: Int): Unit = {
    if (begin < end) {
      val pivotIdx = partition(array, begin, end)
      sequentialSort(array, begin, pivotIdx - 1)
      sequentialSort(array, pivotIdx + 1, end)
    }
  }

  def concurrent(array: Array[Int]): Unit = {
    concurrentSort(array, 0, array.length - 1)
  }

  private def concurrentSort(array: Array[Int], begin: Int, end: Int): Unit = {
    if (begin < end) {
      val pivotIdx = partition(array, begin, end)
      val size = end - begin + 1
      val numOfThreads = Runtime.getRuntime.availableProcessors()

      if (size <= array.length / numOfThreads) {
        sequentialSort(array, begin, pivotIdx - 1)
        sequentialSort(array, pivotIdx + 1, end)
      } else {
        val left = Future(concurrentSort(array, begin, pivotIdx - 1))
        concurrentSort(array, pivotIdx + 1, end)
        Await.ready(left, Duration.Inf)
      }
    }
  }

  private def partition(array: Array[Int], begin: Int, end: Int): Int = {
    val randIdx = Random.nextInt(end - begin) + begin
    swap(array, randIdx, end)
    val pivot = array(end)

    var i = begin - 1
    for (j <- Range(begin, end)) {
      if (array(j) <= pivot) {
        i += 1
        swap(array, i, j)
      }
    }
    swap(array, i + 1, end)

    i + 1
  }

  private def swap(array: Array[Int], i: Int, j: Int): Unit = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }
}
