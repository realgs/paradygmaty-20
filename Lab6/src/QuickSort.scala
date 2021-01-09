import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object QuickSort {

  def quickSortSequential(array: Array[Int]): Array[Int] =
    quickSortSequential(array, 0, array.length)

  def quickSortSequential(array: Array[Int], startIndex: Int, endIndex: Int): Array[Int] = {

    if(endIndex - startIndex > 1) {

      val p = partition(array, startIndex, endIndex)

      quickSortSequential(array, startIndex, p)
      quickSortSequential(array, p + 1, endIndex)
    }

    array
  }

  def quickSortParallel(array: Array[Int]): Array[Int] =
    quickSortParallel(array, 0, array.length)

  def quickSortParallel(array: Array[Int], startIndex: Int, endIndex: Int): Array[Int] = {

    if(endIndex - startIndex > 1) {

      val p = partition(array, startIndex, endIndex)

      Await.result(Future(quickSortSequential(array, startIndex, p)), Duration.Inf)
      Await.result(Future(quickSortSequential(array, p + 1, endIndex)), Duration.Inf)
    }

    array
  }

  def partition(array: Array[Int], nFrom: Int, nTo: Int): Int = {

    val pivot = nFrom + (nTo - nFrom) / 2

    swap(array, nFrom, pivot)

    val value = array(nFrom)

    var idxBigger = nFrom + 1
    var idxLower = nTo - 1

    do {

      while(idxBigger <= idxLower && array(idxBigger) <= value)
        idxBigger += 1

      while(array(idxLower) > value)
        idxLower -= 1

      if(idxBigger < idxLower)
        swap(array, idxBigger, idxLower)

    } while(idxBigger < idxLower)

    swap(array, idxLower, nFrom)

    idxLower
  }

  def swap(array: Array[Int], left: Int, right: Int): Unit =
    if(left != right) {

      val temp = array(left)

      array.update(left, array(right))
      array.update(right, temp)
    }

}
