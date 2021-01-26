// zadanie 3 (1 + 3 pkt)
object zad03 {
  type TreePair[A] = (BinaryTree[A], BinaryTree[A])

  def removeDuplicatesDepth(btA: BinaryTree[Int], btB: BinaryTree[Int]): TreePair[Int] = {
    validate(btA, btB)
    removeDepth(btA, btB)
  }

  private def removeDepth(btA: BinaryTree[Int], btB: BinaryTree[Int]): TreePair[Int] = {
    (btA, btB) match {
      case (Empty, Empty) => (Empty, Empty)
      case (a: Node[Int], b: Node[Int]) =>
        val (leftA, leftB) = removeDepth(a.left, b.left)
        val (rightA, rightB) = removeDepth(a.right, b.right)
        (a.el == b.el, leftA == Empty, rightA == Empty) match {
          case (true, true, true) => (Empty, Empty)
          case (true, _, _) => (Node(-1, leftA, rightA), Node(-1, leftB, rightB))
          case (false, _, _) => (Node(a.el, leftA, rightA), Node(b.el, leftB, rightB))
        }
    }
  }

  def removeDuplicatesBreadth(btA: BinaryTree[Int], btB: BinaryTree[Int]): TreePair[Int] = {
    validate(btA, btB)
    removeBreadth(btA, btB)
  }

  private def removeBreadth(btA: BinaryTree[Int], btB: BinaryTree[Int]): TreePair[Int] = {
    (btA, btB) match {
      case (Empty, Empty) => (Empty, Empty)
      case (a: Node[Int], b: Node[Int]) =>
        val (leftA, leftB) = if (areTreesEqual(a.left, b.left)) (Empty, Empty) else removeBreadth(a.left, b.left)
        val (rightA, rightB) = if (areTreesEqual(a.right, b.right)) (Empty, Empty) else removeBreadth(a.right, b.right)
        (a.el == b.el, leftA == Empty, rightA == Empty) match {
          case (true, true, true) => (Empty, Empty)
          case (true, _, _) => (Node(-1, leftA, rightA), Node(-1, leftB, rightB))
          case (false, _, _) => (Node(a.el, leftA, rightA), Node(b.el, leftB, rightB))
        }
    }
  }

  private def areTreesEqual(btA: BinaryTree[Int], btB: BinaryTree[Int]): Boolean = {
    @scala.annotation.tailrec
    def bfsEqual(queue: List[TreePair[Int]]): Boolean = {
      queue match {
        case Nil => true
        case (Empty, Empty) :: t => bfsEqual(t)
        case (Node(a, aLeft, aRight), Node(b, bLeft, bRight)) :: t =>
          (a == b) && bfsEqual(t ::: List((aLeft, bLeft), (aRight, bRight)))
      }
    }

    bfsEqual(List((btA, btB)))
  }

  private def validate[A](a: BinaryTree[A], b: BinaryTree[A]): Unit = {
    if (Utils.depthOfFullBT(a) != Utils.depthOfFullBT(b)) {
      throw new IllegalArgumentException("Trees must have same depth")
    }
  }
}
