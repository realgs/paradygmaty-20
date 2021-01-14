import scala.util.Random

object Utils {

  sealed trait BinaryTree[+T]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+T](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

  def randomInt(range: (Int, Int)): Int =
    Random.nextInt(range._2 - range._1 + 1) + range._1

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

  def generateBinaryTree(n: Int, range: (Int, Int)): BinaryTree[Int] = {
    def generate(k: Int): BinaryTree[Int] =
      if(k == 0) Empty
      else Node(randomInt(range), generate(k - 1), generate(k - 1))

    if(n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if(range._1 <= 0) throw new IllegalArgumentException("range must be positive")
    else if(range._1 > range._2) throw new IllegalArgumentException("invalid range")
    else generate(n)
  }

}
