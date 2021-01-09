import scala.util.Random

object Utils {

  def generateRandomIntArray(size: Int, min: Int, max: Int): Array[Int] =
    Array.fill(size)(Random.between(min, max))

  def calculateTime[T](operationName: String, block: => T): T = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    printf(s"$operationName time:\t %10d ns%n", t1 - t0)
    result
  }

}
