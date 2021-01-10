import scala.util.Random

object Utils {

  def generateRandomIntArray(size: Int, min: Int, max: Int): Array[Int] =
    Array.fill(size)(Random.between(min, max))

  def calculateTime[T](operationName: String, block: => T): T = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    printf(s"%-20s time:\t %15d ns%n", operationName, t1 - t0)
    result
  }

  def generateRandomMatrix(m: Int, n: Int, min: Int, max: Int): Matrix = {

    val a = Array.ofDim[Double](m, n)

    for(i <- a.indices)
      for(j <- a(0).indices)
        a(i)(j) = Random.between(min, max)

    new Matrix(a)
  }
}
