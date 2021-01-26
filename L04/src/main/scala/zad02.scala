// zadanie 2 (3 pkt)
object zad02 {
  def subtractTrees(a: BinaryTree[Int], b: BinaryTree[Int]): BinaryTree[Int] = {
    if (Utils.depthOfFullBT(a) != Utils.depthOfFullBT(b)) {
      throw new IllegalArgumentException("Trees must have same depth")
    }
    helper(a, b)
  }

  private def helper(a: BinaryTree[Int], b: BinaryTree[Int]): BinaryTree[Int] = {
    (a, b) match {
      case (Empty, Empty) => Empty
      case (Node(x, xLeft, xRight), Node(y, yLeft, yRight)) => Node(x - y, helper(xLeft, yLeft), helper(xRight, yRight))
    }
  }
}
