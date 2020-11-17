object Utils {
  def depthOfFullBT[A](tree: BinaryTree[A]): Int = {
    @scala.annotation.tailrec
    def helper(tree: BinaryTree[A], depth: Int): Int = {
      tree match {
        case Empty => depth
        case Node(_, left, _) => helper(left, depth + 1)
      }
    }
    helper(tree, 0)
  }
}
