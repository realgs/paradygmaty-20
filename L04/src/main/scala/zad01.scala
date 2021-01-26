import scala.util.Random

// zadanie 1 (3 pkt)
object zad01 {
  def createFullBinaryTree(depth: Int, left: Int, right: Int): BinaryTree[Int] = {
    def helper(n: Int): BinaryTree[Int] = {
      if (n == 0) Empty
      else Node(Random.nextInt(right - left) + left, helper(n - 1), helper(n - 1))
    }

    validate(depth, left, right)
    helper(depth)
  }

  private def validate(depth: Int, left: Int, right: Int): Unit = {
    if (depth < 0) throw new IllegalArgumentException("Depth must be positive")
    if (left <= 0 || right <= 0) throw new IllegalArgumentException("Bounds must be positive")
    if (left > right) throw new IllegalArgumentException("Left bound cannot be greater than the right one")
  }
}
