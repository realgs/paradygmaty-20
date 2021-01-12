package L4
import scala.collection.immutable.LazyList.#::
import scala.math.max

object L4 {
  sealed trait BT[A]
  case class Empty[A]() extends BT[A]
  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  // helper function
  def depthBT[A](tree: BT[A]): Int = {
    def innerDepth[A](tree: BT[A], depth: Int): Int =
      tree match {
        case Empty() => depth
        case Node(_, leftSubTree, rightSubTree) => max(innerDepth(leftSubTree, depth + 1), innerDepth(rightSubTree, depth + 1))
      }
    innerDepth(tree, 0)
  }

  // helper function
  def numberOfNodesBT[A](tree: BT[A]): Int =
    tree match {
      case Empty() => 0
      case Node(_, leftSubTree, rightSubTree) => 1 + numberOfNodesBT(leftSubTree) + numberOfNodesBT(rightSubTree)
    }

  // helper function
  def isFullBT[A](tree: BT[A]): Boolean =
    if (numberOfNodesBT(tree) == (Math.pow(2, depthBT(tree)) - 1)) true
    else false

  // helper function
  def DFS_BT [A](tree: BT[A]): List[A] =
    tree match {
      case Empty() => Nil
      case Node(v, leftSubtree, rightSubtree)  => v :: DFS_BT(leftSubtree) ::: DFS_BT(rightSubtree)
    }

  // helper function to test task 1
  @scala.annotation.tailrec
  def areInRange (list: List[Int], rangeFrom: Int, rangeTo: Int): Boolean =
    list match {
      case Nil => true
      case hd :: tl =>
        if (hd >= rangeFrom && hd <= rangeTo) areInRange(tl, rangeFrom, rangeTo)
        else false
    }

  // zdadanie 1 (3 pkt)
  val random = scala.util.Random

  // version with range from 0 do 'range'
  def generateBT(depth: Int, range: Int): BT[Int] =
    if (depth < 0 || range < 0) throw new Exception("Invalid argument: values can't be negative")
    else depth match {
      case 0 => Empty()
      case 1 => Node(random.nextInt(range + 1), Empty(), Empty())
      case depth => Node(random.nextInt(range + 1), generateBT(depth - 1, range), generateBT(depth - 1, range))
    }

  // version with range from 'rangeFrom' to 'rangeTo'
  def generateBT(depth: Int, rangeFrom: Int, rangeTo: Int): BT[Int] =
    if (depth < 0 || rangeFrom > rangeTo) throw new Exception("Invalid arguments: depth can't be negative and rangeFrom can't be greater than rangeTo")
    else if (rangeFrom <= 0) generateBT(depth, rangeTo)
    else depth match {
      case 0 => Empty()
      case 1 => Node(random.nextInt(rangeTo - rangeFrom + 1) + rangeFrom, Empty(), Empty())
      case depth => Node(random.nextInt(rangeTo - rangeFrom + 1) + rangeFrom, generateBT(depth - 1, rangeFrom, rangeTo), generateBT(depth - 1, rangeFrom, rangeTo))
    }

  // zadanie 2 (3 pkt)
  def differenceBT(minuendTree: BT[Int], subtrahendTree: BT[Int]): BT[Int] = {
    if (depthBT(minuendTree) != depthBT(subtrahendTree)) throw new Exception("Invalid arguments: different depths of trees")
    else if (!isFullBT(minuendTree) || !isFullBT(subtrahendTree)) throw new Exception("Invalid arguments: trees must be full")
    else {
      def innerDifferenceBT(minuendTree: BT[Int], subtrahendTree: BT[Int]): BT[Int] =
        (minuendTree, subtrahendTree) match {
          case (Empty(), Empty()) => Empty()
          case (Node(v1, leftSubTree1, rightSubTree1), Node(v2, leftSubTree2, rightSubTree2)) =>
            Node(v1 - v2, innerDifferenceBT(leftSubTree1, leftSubTree2), innerDifferenceBT(rightSubTree1, rightSubTree2))
        }
      innerDifferenceBT(minuendTree, subtrahendTree)
    }
  }

  // zadanie 3 wgłąb (1 pkt)
  // helper function
  def areTheSameBT_DFS[A](tree1: BT[A], tree2: BT[A]): Boolean =
    (tree1, tree2) match {
      case (Empty(), Empty()) => true
      case (Node(v1, leftSubTree1, rightSubTree1), Node(v2, leftSubTree2, rightSubTree2)) =>
        if (v1 != v2) false
        else areTheSameBT_DFS(leftSubTree1, leftSubTree2) && areTheSameBT_DFS(rightSubTree1, rightSubTree2)
    }

  def removeDuplicatesBT_DFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if (depthBT(tree1) != depthBT(tree2)) throw new Exception("Invalid arguments: different depths of trees")
    else if (!isFullBT(tree1) || !isFullBT(tree2)) throw new Exception("Invalid arguments: trees must be full")
    else {
      def innerRemove(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
        (tree1, tree2) match {
          case (Empty(), Empty()) => (Empty(), Empty())
          case (Node(v1, leftSubtree1, rightSubtree1), Node(v2, leftSubtree2, rightSubtree2)) =>
            if (v1 == v2 && areTheSameBT_DFS(leftSubtree1, leftSubtree2) && areTheSameBT_DFS(rightSubtree1, rightSubtree2)) (Empty(), Empty())
            else {
              val leftSubtrees = innerRemove(leftSubtree1, leftSubtree2)
              val rightSubtrees = innerRemove(rightSubtree1, rightSubtree2)
              if (v1 == v2) (Node(-1, leftSubtrees._1, rightSubtrees._1), Node(-1, leftSubtrees._2, rightSubtrees._2))
              else (Node(v1, leftSubtrees._1, rightSubtrees._1), Node(v2, leftSubtrees._2, rightSubtrees._2))
            }
        }
      }
      innerRemove(tree1, tree2)
    }
  }

  // zadanie 3 wszerz (3 pkt)
  // helper function
  def areTheSameBT_BFS[A](tree1: BT[A], tree2: BT[A]): Boolean = {
    @scala.annotation.tailrec
    def innerCompare_BFS[A](treeQueue1: List[BT[A]], treeQueue2: List[BT[A]]): Boolean =
      (treeQueue1, treeQueue2) match {
        case (Nil, Nil) => true
        case (Empty() :: tail1, Empty() :: tail2) => innerCompare_BFS(tail1, tail2)
        case (Node(v1, leftSubTree1, rightSubTree1) :: tail1, Node(v2, leftSubTree2, rightSubTree2) :: tail2) =>
          if (v1 != v2) false
          else innerCompare_BFS(tail1 ::: List(leftSubTree1, rightSubTree1), tail2 ::: List(leftSubTree2, rightSubTree2))
      }
    innerCompare_BFS(List(tree1), List(tree2))
  }

  def removeDuplicatesBT_BFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if (depthBT(tree1) != depthBT(tree2)) throw new Exception("Invalid arguments: different depths of trees")
    else if (!isFullBT(tree1) || !isFullBT(tree2)) throw new Exception("Invalid arguments: trees must be full")
    else {
      def innerRemove(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
        (tree1, tree2) match {
          case (Empty(), Empty()) => (Empty(), Empty())
          case (Node(v1, leftSubtree1, rightSubtree1), Node(v2, leftSubtree2, rightSubtree2)) =>
            if (v1 == v2 && areTheSameBT_BFS(leftSubtree1, leftSubtree2) && areTheSameBT_BFS(rightSubtree1, rightSubtree2)) {
              (Empty(), Empty())
            }
            else if (v1 == v2 && areTheSameBT_BFS(leftSubtree1, leftSubtree2)) {
              val subtrees = innerRemove(rightSubtree1, rightSubtree2)
              (Node(-1, Empty(), subtrees._1), Node(-1, Empty(), subtrees._2))
            }
            else if (v1 == v2 && areTheSameBT_BFS(rightSubtree1, rightSubtree2)) {
              val subtrees = innerRemove(leftSubtree1, leftSubtree2)
              (Node(-1, subtrees._1, Empty()), Node(-1, subtrees._2, Empty()))
            }
            else {
              val leftSubtrees = innerRemove(leftSubtree1, leftSubtree2)
              val rightSubtrees = innerRemove(rightSubtree1, rightSubtree2)
              if (v1 == v2) (Node(-1, leftSubtrees._1, rightSubtrees._1), Node(-1, leftSubtrees._2, rightSubtrees._2))
              else (Node(v1, leftSubtrees._1, rightSubtrees._1), Node(v2, leftSubtrees._2, rightSubtrees._2))
            }
        }
      }
      innerRemove(tree1, tree2)
    }
  }

  // zadanie 4 (5 pkt)
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): LazyList[A] = {
    if (n <= 0 || m <= 0) throw new Exception("Invalid arguments: n and m must be positive")
    else if (lazyList == LazyList()) LazyList()
    else {
      def innerEachNElement[A](lazyList: LazyList[A], n: Int, nIterator: Int, repeats: Int): LazyList[A] =
        (lazyList, nIterator, repeats) match {
          case (head #:: _, nIterator, 1) =>
            if (nIterator == n) head #:: LazyList()
            else LazyList()
          case (head #:: lazyTail, nIterator, repeats) =>
            if (nIterator == n) head #:: innerEachNElement(lazyTail, n, 1, repeats - 1)
            else innerEachNElement(lazyTail, n, nIterator + 1, repeats - 1)
        }
      innerEachNElement(lazyList, n, n, m)
    }
  }

  // zadanie 5 (5 pkt)
  // declaring new type
  sealed trait MathOperator
  case object + extends MathOperator
  case object - extends MathOperator
  case object * extends MathOperator
  case object / extends MathOperator

  // helper function with anonymous function as an argument
  def lMathOperation(lazyList1: LazyList[Int], lazyList2: LazyList[Int], mathOperation: (Int, Int) => Int): LazyList[Int] =
    (lazyList1, lazyList2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (head1 #:: lazyTail1, head2 #:: lazyTail2) => mathOperation (head1, head2) #:: lMathOperation(lazyTail1, lazyTail2, mathOperation)
    }

  def lOperation(lazyList1: LazyList[Int], lazyList2: LazyList[Int], operator: MathOperator): LazyList[Int] = {
    if (lazyList1.length != lazyList2.length) throw new Exception("Invalid argument - lists must have the same length")
    else operator match {
      case + => lMathOperation(lazyList1, lazyList2, (a:Int, b: Int) => a + b)
      case - => lMathOperation(lazyList1, lazyList2, (a:Int, b: Int) => a - b)
      case * => lMathOperation(lazyList1, lazyList2, (a:Int, b: Int) => a * b)
      case / => lMathOperation(lazyList1, lazyList2, (a:Int, b: Int) => a / b)
    }
  }
}

