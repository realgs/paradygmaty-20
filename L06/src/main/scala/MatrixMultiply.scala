import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object MatrixMultiply {
  type Matrix[A] = Array[Array[A]]

  def sequential(a: Matrix[Int], b: Matrix[Int]): Matrix[Int] = {
    val (numOfRows, numOfColumns, result) = init(a, b)

    for (i <- Range(0, numOfRows)) {
      multiplyRow(a, b, result, i, numOfColumns)
    }

    result
  }

  def concurrent(a: Matrix[Int], b: Matrix[Int]): Matrix[Int] = {
    val (numOfRows, numOfColumns, result) = init(a, b)

    var outerLoops: List[Future[Unit]] = List()
    for (i <- Range(0, numOfRows)) {
      outerLoops = Future(multiplyRow(a, b, result, i, numOfColumns)) :: outerLoops
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

  private def multiplyRow(a: Matrix[Int], b: Matrix[Int], result: Matrix[Int], row: Int, numOfColumns: Int): Unit = {
    for (j <- Range(0, numOfColumns)) {
      for (k <- a(row).indices) {
        result(row)(j) = result(row)(j) + a(row)(k) * b(k)(j)
      }
    }
  }
}
