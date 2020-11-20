import scala.annotation.tailrec

object Functions_L4 {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //task 1 (3pkt)
  def generateTree(depth: Int, minNumber: Int, maxNumber: Int): BT[Int] = {
    if (depth < 0) throw new Exception("depth must be positive")
    if (depth == 0) Empty
    else {
      def helper(actualDepth: Int): BT[Int] = {
        if (actualDepth == depth) Empty
        else Node(scala.util.Random.between(minNumber, maxNumber), helper(actualDepth + 1), helper(actualDepth + 1))
      }

      helper(0)
    }
  }

  //functions used in testing
  def checkIfNumbersAreInRange(tree: BT[Int], minNumber: Int, maxNumber: Int): Boolean = {
    tree match {
      case Node(value, left, right) => if (value >= minNumber && value <= maxNumber
        && checkIfNumbersAreInRange(left, minNumber, maxNumber)
        && checkIfNumbersAreInRange(right, minNumber, maxNumber)) true else false
      case Empty => true
    }
  }

  def findDepth(tree: BT[Int]): Int = {
    def helper(tree: BT[Int], depth: Int): Int = {
      tree match {
        case Empty => depth
        case Node(_, left, right) => Math.max(helper(left, depth + 1), helper(right, depth + 1))
      }
    }

    helper(tree, 0)
  }

  def checkIfTreeIsFull[A](tree: BT[A]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, left, right) =>
        def helper(leftTree: BT[A], rightTree: BT[A]): Boolean = {
          (leftTree, rightTree) match {
            case (Empty, Empty) => true
            case (Node(_, left1, right1), Node(_, left2, right2)) => helper(left1, right1) && helper(left2, right2)
            case (_, _) => false
          }
        }

        helper(left, right)
    }
  }
  def breadthBT[A](tree: BT[A]): List[A] = {
    @tailrec
    def breadthBTIter(queue: List[BT[A]], returnList: List[A]): List[A] = {
      queue match {
        case Nil => returnList.reverse
        case Empty :: tail => breadthBTIter(tail, returnList)
        case Node(value, left, right) :: tail => breadthBTIter(tail ::: List(left, right), value :: returnList)
      }
    }

    breadthBTIter(List(tree), Nil)
  }
}
