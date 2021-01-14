import Utils.{BinaryTree, Empty, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object InorderBinaryTree {

  def inorderBinaryTreeSequential[A](tree: BinaryTree[A]): List[A] = tree match {
    case Empty => Nil
    case Node(v, l, r) => inorderBinaryTreeSequential(l) ::: v :: inorderBinaryTreeSequential(r)
  }

  def inorderBinaryTreeParallel[A](tree: BinaryTree[A]): List[A] = tree match {
    case Empty => Nil
    case Node(v, l, r) =>

      val result = for (left <- Future(inorderBinaryTreeSequential(l)); right <- Future(inorderBinaryTreeSequential(r))) yield left ::: v :: right

      Await.result(result, Duration.Inf)
  }

}
