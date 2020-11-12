import scala.util.Random

object Functions {
  Random.setSeed(0)

  def generateTree(depth: Int, valueMin: Int, valueMax: Int): BTree[Int] = {
    if (depth == 0) Empty
    else {
      BTree(Random.between(valueMin, valueMax), generateTree(depth - 1, valueMin, valueMax),
        generateTree(depth - 1, valueMin, valueMax))
    }
  }
}
