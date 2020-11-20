import scala.annotation.tailrec
import scala.util.Random

object L4Lab extends App {

  // Tree
  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]

  // Helpers
  def isTreeFull[A](tree: BT[A]): Boolean = {
    def isTreeFullHelper(left: BT[A], right: BT[A]): Boolean = (left, right) match {
      case (Empty, Empty) => true
      case (Node(_, left1, right1), Node(_, left2, right2)) => isTreeFullHelper(left1, right1) && isTreeFullHelper(left2, right2)
      case (_, _) => false
    }

    tree match {
      case Empty => true
      case Node(_, left, right) => isTreeFullHelper(left, right)
    }
  }

  def checkIfTreesLevelIsTheSame[A](firstTree: BT[A], secondTree: BT[A]): Boolean = {
    @tailrec
    def compareTreesLevels(left: BT[A], right: BT[A]): Boolean = (left, right) match {
      case (Empty, Empty) => true
      case (Node(_, _, _), Empty) => false
      case (Empty, Node(_, _, _)) => false
      case (Node(_, left1, _), Node(_, left2, _)) => compareTreesLevels(left1, left2)
    }

    if (isTreeFull(firstTree) && isTreeFull(secondTree)) {
      compareTreesLevels(firstTree, secondTree)
    } else {
      false
    }
  }

  // Zadanie 1
  // 3 pkt
  def generateTree(treeLevel: Int, minRange: Int, maxRange: Int): BT[Int] = {
    def generateTreeHelper(treeLevel: Int): BT[Int] = treeLevel match {
      case 0 => Empty
      case _ => Node(
        value = Random.nextInt(maxRange - minRange + 1) + minRange,
        left = generateTreeHelper(treeLevel - 1),
        right = generateTreeHelper(treeLevel - 1)
      )
    }

    if (treeLevel < 0) {
      throw new IllegalArgumentException("Tree level must be greater than 0")
    } else if (minRange >= maxRange) {
      throw new IllegalArgumentException("minRange has to be smaller than maxRange")
    } else {
      generateTreeHelper(treeLevel)
    }
  }

  // Zadanie 2
  // 3 pkt
  def subtractTrees(firstTree: BT[Int], secondTree: BT[Int]): BT[Int] = {
    if (checkIfTreesLevelIsTheSame(firstTree, secondTree)) {
      (firstTree, secondTree) match {
        case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(
          value = value1 - value2,
          left = subtractTrees(left1, left2),
          right = subtractTrees(right1, right2)
        )
        case (_, _) => Empty
      }
    } else {
      throw new IllegalArgumentException("Given trees should have the same level")
    }
  }

  // Zadanie 3
  // 1 pkt

  // Wgłąb
  // 1 pkt
  def removeDuplicatesDepth(firstTree: BT[Int], secondTree: BT[Int]): (BT[Int], BT[Int]) = {
    def removeDuplicatesDepthHelper(firstTree: BT[Int], secondTree: BT[Int]): (BT[Int], BT[Int]) = (firstTree, secondTree) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(value1, left1, right1), Node(value2, left2, right2)) =>
        val (leftRemoved1, leftRemoved2) = removeDuplicatesDepthHelper(left1, left2)
        val (rightRemoved1, rightRemoved2) = removeDuplicatesDepthHelper(right1, right2)

        if (value1 == value2) {
          (leftRemoved1, rightRemoved1, leftRemoved2, rightRemoved2) match {
            case (Empty, Empty, Empty, Empty) => (Empty, Empty)
            case _ => (Node(-1, leftRemoved1, rightRemoved1), Node(-1, leftRemoved2, rightRemoved2))
          }
        } else {
          (Node(value1, leftRemoved1, rightRemoved1), Node(value2, leftRemoved2, rightRemoved2))
        }
    }

    if (isTreeFull(firstTree) && isTreeFull(secondTree)) {
      if (checkIfTreesLevelIsTheSame(firstTree, secondTree)) {
        removeDuplicatesDepthHelper(firstTree, secondTree)
      } else {
        throw new IllegalArgumentException("Given trees should have the same level")
      }
    } else {
      throw new IllegalArgumentException("Given trees should be full")
    }
  }

  // Zadanie 4
  // 5 pkt
  def eachNElement[A](lList: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementHelper(lList: LazyList[A], nNumberCounter: Int, otherElementsToTake: Int): LazyList[A] = lList match {
      case head #:: tail => (nNumberCounter, otherElementsToTake) match {
        case (_, 0) => LazyList()
        case (0, _) => head #:: eachNElementHelper(tail, n - 1, otherElementsToTake - 1)
        case (_, _) => eachNElementHelper(tail, nNumberCounter - 1, otherElementsToTake - 1)
      }
      case _ => LazyList()
    }

    if (n <= 0) {
      throw new IllegalArgumentException("Each number value has to be greater than 0")
    } else if (m < 0) {
      throw new IllegalArgumentException("Index of last item cannot be negative")
    } else {
      eachNElementHelper(lList, 0, m)
    }
  }

  // Zadanie 5
  // 5 pkt
  def add(a: Double, b: Double) = a + b

  def subtract(a: Double, b: Double) = a - b

  def multiply(a: Double, b: Double) = a * b

  def divide(a: Double, b: Double) = {
    if (b == 0.0) {
      throw new IllegalArgumentException("Cannot divide by 0!")
    } else {
      a / b
    }
  }

  def lOperation(firstLList: LazyList[Double], secondLList: LazyList[Double], operation: (Double, Double) => Double): LazyList[Double] = {
    (firstLList, secondLList) match {
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), _) => secondLList
      case (_, LazyList()) => firstLList
      case (firstHead #:: firstTail, secondHead #:: secondTail) =>
        operation(firstHead, secondHead) #:: lOperation(firstTail, secondTail, operation)
    }
  }

}
