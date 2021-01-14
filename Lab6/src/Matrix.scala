import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Matrix(private val rows: Array[Array[Double]]) {
  require(checkRows(0, rows), "wrong rows dimensions")

  private val columns = {

    val a = Array.ofDim[Double](rows(0).length, rows.length)

    for(i <- rows(0).indices)
      for(j <- rows.indices)
        a(i)(j) = rows(j)(i)

    a
  }

  @tailrec
  private def checkRows(i: Int, allRows: Array[Array[Double]]): Boolean = {
    if(i >= 0 && i < allRows.length) allRows(i).length == allRows(0).length && checkRows(i+1, allRows)
    else true
  }

  private def multiply(row: Array[Double], column: Array[Double]): Double = {

    @tailrec
    def mul(i: Int, acc: Double): Double = {
      if(i < row.length) mul(i+1, acc+row(i)*column(i))
      else acc
    }

    if(row.length != column.length) throw new IllegalArgumentException("wrong dimensions")
    mul(0, 0.0)
  }

  def multiplySequential(other: Matrix): Matrix = {

    if(columns.length != other.rows.length) throw new IllegalArgumentException("multiply impossible")

    val a = Array.ofDim[Double](rows.length, other.columns.length)

    for(i <- a.indices)
      for(j <- a(0).indices)
        a(i)(j) = multiply(rows(i), other.columns(j))

    new Matrix(a)
  }

  def multiplyParallel(other: Matrix): Matrix = {

    if(columns.length != other.rows.length) throw new IllegalArgumentException("multiply impossible")

    val a = Array.ofDim[Double](rows.length, other.columns.length)
    val f = Array.ofDim[Future[Unit]](rows.length, other.columns.length)


    for(i <- a.indices)
      for(j <- a(0).indices)
        f(i)(j) = Future(a(i)(j) = multiply(rows(i), other.columns(j)))

    f.map(r => r.map(Await.result(_, Duration.Inf)))

    new Matrix(a)
  }

  override def toString: String = {

    val sb = new StringBuilder

    sb.append("\n")
    for(i <- rows.indices) {
      sb.append("[ ")
      for(j <- rows(0).indices) {
        sb.append(rows(i)(j)).append(", ")
      }
      sb.delete(sb.length()-2, sb.length())
      sb.append(" ]\n")
    }
    sb.append("\n")
    sb.toString()
  }

  private def canEqual(a: Any): Boolean = a.isInstanceOf[Matrix]

  override def equals(obj: Any): Boolean = obj match {
    case obj: Matrix => obj.canEqual(this) && {

      var b = true

      if(rows.length != obj.rows.length || columns.length != obj.columns.length) b = false
      else
        for (i <- rows.indices)
          for (j <- rows(0).indices)
            if(rows(i)(j) != obj.rows(i)(j))
              b = false
      b
    }
    case _ => false
  }

}

object Matrix {

  def apply(a: Array[Array[Double]]) = new Matrix(a)
}
