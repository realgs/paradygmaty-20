import java.util.concurrent.{ForkJoinPool, RecursiveAction}
import java.util.concurrent.ForkJoinTask.invokeAll

object SqrtElements {
  def sequential(array: Array[Double]): Unit = sqrtElements(array, 0, array.length)

  def parallel(array: Array[Double]): Unit = new ForkJoinPool().invoke(new SqrtElementsParallel(array, 0, array.length))

  private def sqrtElements(array: Array[Double], start: Int, end: Int): Unit = {
    var i = start

    while (i < end) {
      array(i) = Math.sqrt(array(i))
      i += 1
    }
  }

  private class SqrtElementsParallel(array: Array[Double], start: Int, end: Int) extends RecursiveAction {
    private val THRESHOLD = 100_000

    override def compute(): Unit = {
      if (end - start < THRESHOLD) {
        sqrtElements(array, start, end)
      } else {
        val mid = (start + end) / 2
        invokeAll(new SqrtElementsParallel(array, start, mid), new SqrtElementsParallel(array, mid, end))
      }
    }
  }

}
