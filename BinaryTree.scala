package L6
import ParallelComputing.parallel

object BinaryTree {
  sealed trait BT[A]
  case class Empty[A]() extends BT[A]
  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  private val random = scala.util.Random

  def generateBT(depth: Int, range: Int): BT[Int] =
    if (depth < 0 || range < 0) throw new Exception("Invalid argument: values can't be negative")
    else depth match {
      case 0 => Empty()
      case 1 => Node(random.nextInt(range + 1), Empty(), Empty())
      case depth => Node(random.nextInt(range + 1), generateBT(depth - 1, range), generateBT(depth - 1, range))
    }

  def numberOfElementBT [A](tree: BT[A], element: A): Int =
    tree match {
      case Empty() => 0
      case Node(e, leftSubTree, rightSubTree) =>
        if (e == element) 1 + numberOfElementBT(leftSubTree, element) + numberOfElementBT(rightSubTree, element)
        else numberOfElementBT(leftSubTree, element) + numberOfElementBT(rightSubTree, element)
    }

  def numberOfElementBTParallel [A](tree: BT[A], element: A): Int =
    tree match {
      case Empty() => 0
      case Node(e, leftSubTree, rightSubTree) => {
        val (leftResult, rightResult) = parallel(numberOfElementBT(leftSubTree, element), numberOfElementBT(rightSubTree, element))
        if (e == element) 1 + leftResult + rightResult
        else leftResult + rightResult
      }
    }


  def areTheSameBT[A](tree1: BT[A], tree2: BT[A]): Boolean =
    (tree1, tree2) match {
      case (Empty(), Empty()) => true
      case (Node(v1, leftSubTree1, rightSubTree1), Node(v2, leftSubTree2, rightSubTree2)) =>
        if (v1 != v2) false
        else areTheSameBT(leftSubTree1, leftSubTree2) && areTheSameBT(rightSubTree1, rightSubTree2)
    }

  def areTheSameBTParallel[A](tree1: BT[A], tree2: BT[A]): Boolean =
    (tree1, tree2) match {
      case (Empty(), Empty()) => true
      case (Node(v1, leftSubTree1, rightSubTree1), Node(v2, leftSubTree2, rightSubTree2)) =>
        if (v1 != v2) false
        else {
          val (leftSubTreeCheck, rightSubTreeCheck) = parallel(areTheSameBT(leftSubTree1, leftSubTree2), areTheSameBT(rightSubTree1, rightSubTree2))
          leftSubTreeCheck && rightSubTreeCheck
        }
    }
}

