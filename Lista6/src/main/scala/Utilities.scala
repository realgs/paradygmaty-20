import Martix.Matrix
import scala.util.Random

object Utilities {
  val random: Random.type = scala.util.Random

  def timeMeasureMilliSeconds[A](block: => A): Unit = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    println("Total time: " + (t1 - t0) + "ms")
  }

  def timeMeasureNanoSeconds[A](block: => A): Unit = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Total time: " + (t1 - t0) + "ns")
  }

  def createMatrixWithRandomIntegers(rowsAmount: Int, columnsAmount: Int): Matrix = {
    require(rowsAmount > 0, s"Matrix can't be empty: rows length is 0 or negative")
    require(columnsAmount > 0, s"Matrix can't be empty: columns length is 0 or negative")
    val newMatrix = Array.ofDim[Int](rowsAmount, columnsAmount)
    for(i <- 0 until rowsAmount) {
      for(j <- 0 until columnsAmount) {
        newMatrix(i)(j) = random.nextInt(100) - 50
      }
    }
    new Matrix(newMatrix)
  }

}
