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

  //task 2 (3pkt)
  def diffTrees(firstTree: BT[Int], secondTree: BT[Int]): BT[Int] = {
    def helper(firstTree: BT[Int], secondTree: BT[Int]): BT[Int] = {
      (firstTree, secondTree) match {
        case (Node(_, _, _), Empty) | (Empty, Node(_, _, _)) => throw new Exception("trees have different depths")
        case (Empty, Empty) => Empty
        case (Node(firstElement, firstLeft, firstRight), Node(secondElement, secondLeft, secondRight)) =>
          Node(firstElement - secondElement, helper(firstLeft, secondLeft), helper(firstRight, secondRight))
      }
    }

    helper(firstTree, secondTree)
  }

  //task 4 (5pkt)
  def eachNElement[A](lazyList: LazyList[A], n: Int, lastIndex: Int): LazyList[A] = {
    if (n == 0) LazyList()
    else if (n < 0 || lastIndex < 1) throw new Exception("invalid value")
    else {
      def helper(lazyList: LazyList[A], k: Int, lastIndex: Int): LazyList[A] = {
        if (lastIndex == 0) LazyList()
        else {
          (lazyList, k) match {
            case (LazyList(), _) => LazyList()
            case (head #:: tail, 1) => head #:: helper(tail, n, lastIndex - 1)
            case (_ #:: tail, _) => helper(tail, k - 1, lastIndex - 1)
          }
        }
      }

      helper(lazyList, 1, lastIndex)
    }
  }

  //task 5 (5pkt)
  def ldzialanie(firstLazyList: LazyList[Double], secondLazyList: LazyList[Double], action: (Double, Double) => Double): LazyList[Double] = {
    (firstLazyList, secondLazyList) match {
      case (LazyList(), _) => secondLazyList
      case (_, LazyList()) => firstLazyList
      case (firstHead #:: firstTail, secondHead #:: secondTail) => action(firstHead, secondHead) #:: ldzialanie(firstTail, secondTail, action)
    }
  }

  def addition(x: Double, y: Double): Double = x + y

  def subtraction(x: Double, y: Double): Double = x - y

  def multiplication(x: Double, y: Double): Double = x * y

  def division(x: Double, y: Double): Double = {
    if (y == 0) throw new Exception("division by zero") else x / y
  }
}
