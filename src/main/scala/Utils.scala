import scala.annotation.tailrec

object Utils {

  def calculateDepth[A](tree: BT[A]): Int = {
    @tailrec
    def calculateDepthHelper(currentDepth: Int, actualSubtree: BT[A]): Int = actualSubtree match {
      case Empty => currentDepth
      case Node(_, _, rightBT) => calculateDepthHelper(currentDepth + 1, rightBT)
    }

    calculateDepthHelper(0, tree)
  }

  def isTreeFull[A](tree: BT[A]) : Boolean = tree match {
    case Empty => true
    case Node(_, leftBT, rightBT) => {
      if(leftBT == Empty && rightBT == Empty) true
      else if(leftBT != Empty && rightBT != Empty) isTreeFull(leftBT) && isTreeFull(rightBT)
      else false
    }
  }

  def breadthSearch[A](tree: BT[A]): List[A] = {
    @tailrec
    def breadthSearchHelper(toVisitQueue: List[BT[A]], resultList: List[A]): List[A] = toVisitQueue match {
      case Nil => resultList.reverse
      case Empty :: tail => breadthSearchHelper(tail, resultList)
      case Node(value, leftSubtree, rightSubtree) :: tail => breadthSearchHelper(tail ::: List(leftSubtree, rightSubtree), value :: resultList)
    }

    breadthSearchHelper(List(tree), Nil)
  }
}
