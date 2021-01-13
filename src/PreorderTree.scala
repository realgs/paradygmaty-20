import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random

object PreorderTree {
  trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTree(n: Int, leftThreshold: Int, rightThreshold: Int): BT[Int] = {
    if(leftThreshold <= 0 || rightThreshold <= 0 || leftThreshold >= rightThreshold || n <= 0) Empty
    else if (n == 1) Node(Random.between(leftThreshold, rightThreshold), Empty, Empty)
    else Node(Random.between(leftThreshold, rightThreshold), generateTree(n - 1, leftThreshold, rightThreshold), generateTree(n - 1, leftThreshold, rightThreshold))
  }

  def preorderTraversal[A] (tree: BT[A]): List[A] =
    tree match {
      case Node(value, left, right) => value :: preorderTraversal(left) ::: preorderTraversal(right)
      case Empty => Nil
    }

  def preorderTraversalParallel[A] (tree: BT[A]): List[A] =
    tree match {
      case Node(value, left, right) =>
        val (leftSubTree, rightSubTree) = parallel(preorderTraversalParallel(left), preorderTraversalParallel(right))
        value :: leftSubTree ::: rightSubTree
      case Empty => Nil
    }

  def preorderTraversalFuture[A] (tree: BT[A]): List[A] =
    tree match {
      case Node(value, left, right) =>
        val result = for {
          l <- Future(preorderTraversal(left))
          r <- Future(preorderTraversal(right))
        } yield value :: l ::: r
        Await.result(result, 1000.second)
      case Empty => Nil
    }
}

