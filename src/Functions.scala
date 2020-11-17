import Utils.{height, isTreeFull}

import scala.annotation.tailrec
import scala.util.Random

object Functions {
  trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // Task 1 (3p)
  def generateTree(n: Int, leftThreshold: Int, rightThreshold: Int): BT[Int] =
    if(leftThreshold <= 0 || rightThreshold <= 0 || leftThreshold >= rightThreshold || n <= 0) Empty
    else if (n == 1) Node(Random.between(leftThreshold, rightThreshold), Empty, Empty)
    else Node(Random.between(leftThreshold, rightThreshold), generateTree(n - 1, leftThreshold, rightThreshold), generateTree(n - 1, leftThreshold, rightThreshold))

  // Task 2 (3p)
  def findTreesDifference(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    if (height(tree1) != height(tree2) || !isTreeFull(tree1) || !isTreeFull(tree2)) throw new Exception("Trees should be of the same height and must be full.")
    else (tree1, tree2) match {
      case (Node(t1, l1, r1), Node(t2, l2, r2)) => Node(t1 - t2, findTreesDifference(l1, l2), findTreesDifference(r1, r2))
      case _ => Empty
    }

  // Task 3 (1 + 3)p
  def removeDuplicatesDFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
    if (height(tree1) != height(tree2) || !isTreeFull(tree1) || !isTreeFull(tree2)) throw new Exception("Trees should be of the same height and must be full.")
    else (tree1, tree2) match {
      case (Node(t1, Empty, Empty), Node(t2, Empty, Empty)) =>
        if (t1 == t2) (Empty, Empty) else (Node(t1, Empty, Empty), Node(t2, Empty, Empty))
      case (Node(t1, l1, r1), Node(t2, l2, r2)) =>
        val (subLeft1, subLeft2) = removeDuplicatesDFS(l1, l2)
        val (subRight1, subRight2) = removeDuplicatesDFS(r1, r2)

        (t1 == t2, (subLeft1, subLeft2, subRight1, subRight2)) match {
          case (true, (Empty, Empty, Empty, Empty)) => (Empty, Empty) // oznacza ze drzewa sa takie same
          case (true, _) => (Node(-1, subLeft1, subRight1), Node(-1, subLeft2, subRight2)) // wartosci wezla sa takie same ale poddrzewa nie
          case (false, _) => (Node(t1, subLeft1, subRight1), Node(t2, subLeft2, subRight2))
        }
      case _ => (Empty, Empty)
    }

  def removeDuplicatesBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    def breadthSearch(t1: BT[Int], t2: BT[Int]): Boolean = {
      @tailrec
      def breadthSearchHelper(queue: List[(BT[Int], BT[Int])]): Boolean =
        queue match {
          case Nil => true
          case (Empty, Empty) :: _ => true
          case (Node(v1, l1, r1), Node(v2, l2, r2)) :: tail =>
            (v1 == v2) && breadthSearchHelper(tail ::: List((l1, l2), (r1, r2)))
        }
      breadthSearchHelper(List((t1, t2)))
    }

    def constructTree(t1: BT[Int], t2: BT[Int], queueResult: (BT[Int], BT[Int]) => Boolean): (BT[Int], BT[Int]) =
      (t1, t2) match {
        case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
          (queueResult(l1, l2), queueResult(r1, r2)) match {
            case (true, true) => if (v1 == v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
            case (false, true) =>
              val (leftSubtree, rightSubtree) = constructTree(l1, l2, queueResult)
              (Node(if (v1 == v2) -1 else v1, leftSubtree, Empty), Node(if (v1 == v2) -1 else v2, rightSubtree, Empty))
            case (true, false) =>
              val (leftSubtree, rightSubtree) = constructTree(r1, r2, queueResult)
              (Node(if (v1 == v2) -1 else v1, Empty, leftSubtree), Node(if (v1 == v2) -1 else v2, Empty, rightSubtree))
            case _ =>
              val (leftSubtree1, leftSubtree2) = constructTree(l1, l2, queueResult)
              val (rightSubtree1, rightSubtree2) = constructTree(r1, r2, queueResult)
              (Node(if (v1 == v2) -1 else v1, leftSubtree1, rightSubtree1), Node(if (v1 == v2) -1 else v2, leftSubtree2, rightSubtree2))
          }
      }
    if (height(tree1) != height(tree2) || !isTreeFull(tree1) || !isTreeFull(tree2)) throw new Exception("Trees should be of the same height and must be full.")
    else constructTree(tree1, tree2, (tree1, tree2) => breadthSearch(tree1, tree2))
  }

    // Task 4 (5p)
  def eachNElement[A] (list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementHelper(result: LazyList[A], currentIndex: Int): LazyList[A] =
      if(currentIndex == m) LazyList()
      else result match {
        case head #:: tail =>
          if(currentIndex % n == 0) head #:: eachNElementHelper(tail, currentIndex + 1) // currentIndex % n == 0 - sprawdzenie czy currentIndex jest wielokrotnoscia n
          else eachNElementHelper(tail, currentIndex + 1)
        case _ => LazyList()
      }

    if (list == LazyList() || n <= 0 || m <= 0) LazyList()
    else eachNElementHelper(list, 0)
  }

  def eachNElementTailRec[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    @tailrec
    def eachNElementHelper(result: LazyList[A], currentList: LazyList[A], counter: Int, currentIndex: Int): LazyList[A] =
      if (currentIndex == m) result
      else currentList match {
        case head #:: tail =>
          if (counter == 1) eachNElementHelper( head #:: result, tail, n, currentIndex + 1)
          else eachNElementHelper(result, tail, counter - 1, currentIndex + 1)
        case LazyList() => result
      }

    if (list == LazyList() || n <= 0 || m <= 0) LazyList()
    else eachNElementHelper(LazyList(list.head), list.tail, n, 1).reverse
  }

  // Task 5 (5 p)
  def ldzialanie[A] (l1: LazyList[A], l2: LazyList[A], operator: (A, A) => A): LazyList[A] = {
    @tailrec
    def helper(resultList: LazyList[A], list1: LazyList[A], list2: LazyList[A]): LazyList[A] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => resultList.reverse;
        case (LazyList(), h2 #:: t2) => helper(h2 #:: resultList, LazyList(), t2)
        case (h1 #:: t1, LazyList()) => helper(h1 #:: resultList, LazyList(), t1)
        case (h1 #:: t1, h2 #:: t2) => helper(operator(h1, h2) #:: resultList, t1, t2)
      }

    if(l1 == LazyList()) l2
    else if (l2 == LazyList()) l1
    else helper(LazyList(), l1, l2)
  }
}
