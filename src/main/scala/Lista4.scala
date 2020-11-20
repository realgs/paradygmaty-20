import java.lang.Exception

import scala.annotation.tailrec
import scala.util.Random

object Lista4 extends App {

  // zadanie 1 (3 punkty)
  def generateRandomTree(depth: Int, lowerNumberBound: Int, upperNumberBound: Int): BT[Int] = {
    def generateRandomTreeHelper(actualDepth: Int): BT[Int] = {
      if (actualDepth == depth) Empty
      else Node(Random.between(lowerNumberBound, upperNumberBound + 1), generateRandomTreeHelper(actualDepth + 1), generateRandomTreeHelper(actualDepth + 1))
    }

    if (lowerNumberBound <= 0 || upperNumberBound <= 0) throw new IllegalArgumentException("Non positive numbers are not allowed")
    if (lowerNumberBound > upperNumberBound) throw new IllegalArgumentException("Lower bound cannot be greater than upper")

    if (depth < 0) throw new IllegalArgumentException("Depth is not allowed to be negative")
    else if (depth == 0) Empty
    else if (depth == 1) Node(Random.between(lowerNumberBound, upperNumberBound + 1), Empty, Empty)
    else generateRandomTreeHelper(0)
  }

  // zadanie 2 (3 punkty)
  def treesDifference(tree1: BT[Int], tree2: BT[Int]): BT[Int] = {
    def treesDifferenceHelper(tree1: BT[Int], tree2: BT[Int]): BT[Int] = (tree1, tree2) match {
      case (Empty, Empty) => Empty
      case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, treesDifferenceHelper(left1, left2), treesDifferenceHelper(right1, right2))
    }

    if (!Utils.isTreeFull(tree1) && !Utils.isTreeFull(tree2)) throw new IllegalArgumentException("One of trees are not full")
    else if (Utils.calculateDepth(tree1) != Utils.calculateDepth(tree2)) throw new IllegalArgumentException("Trees must have same depths")
    else treesDifferenceHelper(tree1, tree2)
  }

  // zadanie 3 przez wyszukiwanie wglab (1 punkt)
  def removeDuplicatesDepth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    def removeDuplicatesDepthHelper(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(value1, left1, right1), Node(value2, left2, right2)) => {
        val leftResult = removeDuplicatesDepthHelper(left1, left2)
        val rightResult = removeDuplicatesDepthHelper(right1, right2)

        if (value1 == value2) {
          if ((leftResult._1 == Empty || leftResult._2 == Empty) && (rightResult._1 == Empty || rightResult._2 == Empty)) {
            (Empty, Empty)
          }
          else (Node(-1, leftResult._1, rightResult._1), Node(-1, leftResult._2, rightResult._2))
        }
        else {
          (Node(value1, leftResult._1, rightResult._1), Node(value2, leftResult._2, rightResult._2))
        }
      }
    }

    if (!Utils.isTreeFull(tree1) && !Utils.isTreeFull(tree2)) throw new IllegalArgumentException("One of trees are not full")
    else if (Utils.calculateDepth(tree1) != Utils.calculateDepth(tree2)) throw new IllegalArgumentException("Trees must have same depths")
    else removeDuplicatesDepthHelper(tree1, tree2)
  }


  // zadanie 3 przez wyszukiwanie wszerz (3 punkty)
  val EMPTY_ELEMENT = Int.MaxValue

  def removeDuplicatesBreadth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    @tailrec
    def breadthSearch(toVisitQueue1: List[BT[Int]], toVisitQueue2: List[BT[Int]], resultList1: List[Int], resultList2: List[Int]): (List[Int], List[Int]) = (toVisitQueue1, toVisitQueue2) match {
      case (Nil, Nil) => (resultList1.reverse, resultList2.reverse)
      case (Empty :: tail1, Empty :: tail2) => breadthSearch(tail1, tail2, EMPTY_ELEMENT :: resultList1, EMPTY_ELEMENT :: resultList2)
      case (Node(value1, left1, right1) :: tail1, Node(value2, left2, right2) :: tail2) => {
        if (value1 == value2) {
          breadthSearch(tail1 ::: List(left1, right1), tail2 ::: List(left2, right2), -1 :: resultList1, -1 :: resultList2)
        }
        else {
          breadthSearch(tail1 ::: List(left1, right1), tail2 ::: List(left2, right2), value1 :: resultList1, value2 :: resultList2)
        }
      }
    }

    def createTreeFromList(list1: List[Int], list2: List[Int]): (BT[Int], BT[Int]) = {
      val array1 = EMPTY_ELEMENT +: list1.toArray
      val array2 = EMPTY_ELEMENT +: list2.toArray

      def filterArrays(currentIndex: Int): Unit = {
        if (((currentIndex + 1) * 2 + 1) <= array1.length) {
          filterArrays(currentIndex + 1)
        }

        if ((array1(currentIndex * 2) == EMPTY_ELEMENT && array1(currentIndex * 2 + 1) == EMPTY_ELEMENT) || (array2(currentIndex * 2) == EMPTY_ELEMENT && array2(currentIndex * 2 + 1) == EMPTY_ELEMENT)) {
          if (array1(currentIndex) == array2(currentIndex)) {
            array1(currentIndex) = EMPTY_ELEMENT
            array2(currentIndex) = EMPTY_ELEMENT
          }
        }
      }

      def createTreeFromArray(currentIndex: Int): (BT[Int], BT[Int]) = {
        if (array1(currentIndex) == EMPTY_ELEMENT && array2(currentIndex) == EMPTY_ELEMENT) {
          (Empty, Empty)
        }
        else {
          val left = createTreeFromArray(currentIndex * 2)
          val right = createTreeFromArray(currentIndex * 2 + 1)

          (Node(array1(currentIndex), left._1, right._1), Node(array2(currentIndex), left._2, right._2))
        }
      }

      filterArrays(1)
      createTreeFromArray(1)
    }

    if (!Utils.isTreeFull(tree1) && !Utils.isTreeFull(tree2)) throw new IllegalArgumentException("One of trees are not full")
    else if (Utils.calculateDepth(tree1) != Utils.calculateDepth(tree2)) throw new IllegalArgumentException("Trees must have same depths")
    else {
      val lists = breadthSearch(List(tree1), List(tree2), Nil, Nil)
      createTreeFromList(lists._1, lists._2)
    }
  }

  // zadanie 4 (5 punktów)
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def helper(lazyList: LazyList[A], currentIndex: Int): LazyList[A] = lazyList match {
      case LazyList() => LazyList()
      case head #:: tail => {
        if (currentIndex > (m - 1)) LazyList()
        else if (currentIndex % n == 0) head #:: helper(tail, currentIndex + 1)
        else helper(tail, currentIndex + 1)
      }
    }

    if (n < 0 || m < 0) throw new IllegalArgumentException("Input parameters is not allowed to be negative")

    if (n == 0) LazyList()
    else if (n == 1) lazyList
    else helper(lazyList, 0)
  }

  // zadanie 5 (5 punktów)
  def lazyOperation[A](lazyList1: LazyList[A], lazyList2: LazyList[A])(operation: (A, A) => A): LazyList[A] = {
    def helper(lazyList1: LazyList[A], lazyList2: LazyList[A]): LazyList[A] = (lazyList1, lazyList2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (lazyList1, LazyList()) => lazyList1
      case (LazyList(), lazyList2) => lazyList2
      case (head1 #:: tail1, head2 #:: tail2) => operation(head1, head2) #:: helper(tail1, tail2)
    }

    helper(lazyList1, lazyList2)
  }
}
