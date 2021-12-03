import Utils._

import scala.annotation.tailrec

object Functions {

  // Task 1 (3 pkt)
  def generateBinaryTree(n: Int, range: (Int, Int)): BinaryTree[Int] = {
    def generate(k: Int): BinaryTree[Int] =
      if(k == 0) Empty
      else Node(randomInt(range), generate(k - 1), generate(k - 1))

    if(n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if(range._1 <= 0) throw new IllegalArgumentException("range must be positive")
    else if(range._1 > range._2) throw new IllegalArgumentException("invalid range")
    else generate(n)
  }

  // Task 2 (3 pkt)
  def subtractTrees(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): BinaryTree[Int] =
    if(!isFullTree(tree1) || !isFullTree(tree2)) throw new IllegalArgumentException("both trees must be full")
    else if(height(tree1) != height(tree2)) throw new IllegalArgumentException("both trees must be the same height")
    else (tree1, tree2) match {
      case (Empty, Empty) => Empty
      case (Node(v1, left1, right1), Node(v2, left2, right2)) => Node(v1 - v2, subtractTrees(left1, left2), subtractTrees(right1, right2))
    }

  // Task 3 (4 pkt)
  def deleteDuplicatesDFS(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) = {
    def delete(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) =
      (tree1, tree2) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(v1, left1, right1), Node(v2, left2, right2)) =>

          val (newLeft1, newLeft2) = delete(left1, left2)
          val (newRight1, newRight2) = delete(right1, right2)

          if (v1 == v2 && newLeft1 == Empty && newRight1 == Empty) (Empty, Empty)
          else if (v1 == v2) (Node(-1, newLeft1, newRight1), Node(-1, newLeft2, newRight2))
          else (Node(v1, newLeft1, newRight1), Node(v2, newLeft2, newRight2))
      }

    if(!isFullTree(tree1) || !isFullTree(tree2)) throw new IllegalArgumentException("both trees must be full")
    else if(height(tree1) != height(tree2)) throw new IllegalArgumentException("both trees must be the same height")
    else delete(tree1, tree2)
  }

  def deleteDuplicatesBFS(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) = {

    def ifDuplicatesBFS(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): Boolean = {
      @tailrec
      def BFS(queue: List[(BinaryTree[Int], BinaryTree[Int])]): Boolean = queue match {
        case Nil => true
        case (Empty, Empty) :: _ => true
        case (Node(v1, left1, right1), Node(v2, left2, right2)) :: tail => v1 == v2 && BFS(tail ::: List((left1, left2), (right1, right2)))
      }
      BFS(List((tree1, tree2)))
    }

    def constructTree(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) =
      (tree1, tree2) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(v1, left1, right1), Node(v2, left2, right2)) =>
          if (ifDuplicatesBFS(left1, left2) && ifDuplicatesBFS(right1, right2)) {

            if (v1 == v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))

          } else if (ifDuplicatesBFS(left1, left2)) {

            val (newRightTree1, newRightTree2) = constructTree(right1, right2)
            (Node(if (v1 == v2) -1 else v1, Empty, newRightTree1), Node(if (v1 == v2) -1 else v2, Empty, newRightTree2))

          } else if (ifDuplicatesBFS(right1, right2)) {

            val (newLeftTree1, newLeftTree2) = constructTree(left1, left2)
            (Node(if (v1 == v2) -1 else v1, newLeftTree1, Empty), Node(if (v1 == v2) -1 else v2, newLeftTree2, Empty))

          } else {

            val (newLeftTree1, newLeftTree2) = constructTree(left1, left2)
            val (newRightTree1, newRightTree2) = constructTree(right1, right2)
            (Node(if (v1 == v2) -1 else v1, newLeftTree1, newRightTree1), Node(if (v1 == v2) -1 else v2, newLeftTree2, newRightTree2))
          }
      }

    if(!isFullTree(tree1) || !isFullTree(tree2)) throw new IllegalArgumentException("both trees must be full")
    else if(height(tree1) != height(tree2)) throw new IllegalArgumentException("both trees must be the same height")
    else constructTree(tree1, tree2)
  }

  // Task 4 (5 pkt)
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementHelper(list: LazyList[A], i: Int): LazyList[A] =
      if(list.isEmpty || i >= m) LazyList()
      else if(i % n == 0) list.head #:: eachNElementHelper(list.tail, i + 1)
      else eachNElementHelper(list.tail, i + 1)

    if(n < 1) throw new IllegalArgumentException("n must be positive")
    else if(m < 1) throw new IllegalArgumentException("m must be positive")
    eachNElementHelper(list, 0)
  }

  // Task 5 (5 pkt)
  def ldzialanie(list1: LazyList[Double], list2: LazyList[Double], operation: (Double, Double) => Double): LazyList[Double] =
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (_, LazyList()) => list1
      case (LazyList(), _) => list2
      case (h1 #:: t1, h2 #:: t2) => operation(h1, h2) #:: ldzialanie(t1, t2, operation)
    }
}
