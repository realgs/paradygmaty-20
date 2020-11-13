import scala.annotation.tailrec

object Utils {

  // we assume that every left subtree has the same depth as right tree
  def calculateDepth[A](tree: BT[A]) : Int = {
    @tailrec
    def helper(currentDepth: Int, actualSubtree: BT[A]) : Int = actualSubtree match {
      case Empty => currentDepth
      case Node(_, _, rightBT) => helper(currentDepth + 1, rightBT)
    }
    helper(0, tree)
  }

  def breadthSearch[A](tree: BT[A]): List[A] = {
    @tailrec
    def breadthSearchHelper(btQueue: List[BT[A]], resultList: List[A]): List[A] = btQueue match {
      case Nil => resultList.reverse
      case Empty :: tail => breadthSearchHelper(tail, resultList)
      case Node(value, leftSubtree, rightSubtree) :: tail => breadthSearchHelper(tail ::: List(leftSubtree, rightSubtree), value :: resultList)
    }

    breadthSearchHelper(List(tree), Nil)
  }
}
