import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinPool, RecursiveAction}

object QuickSort {

  def sequential(array: Array[Int]): Unit = sortSequential(array, 0, array.length - 1)

  def parallel(array: Array[Int]): Unit = new ForkJoinPool().invoke(new QuickSortParallel(array, 0, array.length - 1))

  private def choosePivot(array: Array[Int], start: Int, end: Int): Int = array((start + end) / 2)

  private def swap[A](array: Array[A], first: Int, second: Int): Unit = {
    val temp = array(first)

    array(first) = array(second)
    array(second) = temp
  }

  private def partition(array: Array[Int], start: Int, end: Int): (Int, Int) = {
    var i = start
    var j = end
    val pivot = choosePivot(array, i, j)

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

  private def sortSequential(array: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val (i, j) = partition(array, start, end)

      if (j - start < end - i) {
        sortSequential(array, start, j)
        sortSequential(array, i, end)
      } else {
        sortSequential(array, i, end)
        sortSequential(array, start, j)
      }
    }
  }

  private class QuickSortParallel(array: Array[Int], start: Int, end: Int) extends RecursiveAction {
    private val THRESHOLD = 1000

    override def compute(): Unit = {
      if (end - start + 1 <= THRESHOLD) {
        sortSequential(array, start, end)
      } else {
        if (start < end) {
          val (i, j) = partition(array, start, end)

          if (j - 1 < end - 1) {
            invokeAll(new QuickSortParallel(array, start, j), new QuickSortParallel(array, i, end))
          } else {
            invokeAll(new QuickSortParallel(array, i, end), new QuickSortParallel(array, start, j))
          }
        }
      }
    }
  }

}
