import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object MatrixMultiply {
  type Matrix[A] = Array[Array[A]]

  def sequential(a: Matrix[Int], b: Matrix[Int]): Matrix[Int] = {
    val (numOfRows, numOfColumns, result) = init(a, b)
    multiplyRows(a, b, result, numOfColumns, (0, numOfRows))
    result
  }

  def concurrent(a: Matrix[Int], b: Matrix[Int]): Matrix[Int] = {
    val (numOfRows, numOfColumns, result) = init(a, b)
    val numOfThreads = Runtime.getRuntime.availableProcessors()
    val step = Math.ceil(numOfRows.toDouble / numOfThreads).toInt

    var outerLoops: List[Future[Unit]] = List()
    for (i <- Range(0, numOfThreads)) {
      val left = i * step
      val right = Math.min(numOfRows, (i + 1) * step)
      outerLoops = Future(multiplyRows(a, b, result, numOfColumns, (left, right))) :: outerLoops
    }
    outerLoops.foreach(future => Await.ready(future, Duration.Inf))

    result
  }

  private def init(a: Matrix[Int], b: Matrix[Int]): (Int, Int, Matrix[Int]) = {
    validate(a, b)

    val numOfRows = a.length
    val numOfColumns = b(0).length
    val result = Array.fill(numOfRows)(Array.fill(numOfColumns)(0))
    (numOfRows, numOfColumns, result)
  }

  private def validate(a: Matrix[Int], b: Matrix[Int]): Unit = {
    if (!(a.length > 0 && a(0).length == b.length)) {
      throw new IllegalArgumentException()
    }
  }

  private def multiplyRows(a: Matrix[Int], b: Matrix[Int], result: Matrix[Int],
                           numOfColumns: Int, rows: (Int, Int)): Unit = {
    val (left, right) = rows
    for (i <- Range(left, right)) {
      for (j <- Range(0, numOfColumns)) {
        for (k <- a(i).indices) {
          result(i)(j) = result(i)(j) + a(i)(k) * b(k)(j)
        }
      }
    }
  }
}
