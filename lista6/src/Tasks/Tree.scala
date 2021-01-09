package Tasks

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Tree {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def areTreesTheSameParallel[A](tree1: BT[A], tree2: BT[A]): Boolean = {
    (tree1, tree2) match {
      case (Empty, Empty) => true
      case (Node(value1, left1, right1), Node(value2, left2, right2)) =>
        if (value1 == value2) {
          val b1 = Future {areTreesTheSame(right1, right2)}
          val b2 = Future {areTreesTheSame(left1, left2)}
          Await.result(b1, 100.seconds) && Await.result(b2, 100.seconds)
        }
        else false
      case (_, _) => false
    }
  }

  def areTreesTheSame[A](tree1: BT[A], tree2: BT[A]): Boolean = {
    (tree1, tree2) match {
      case (Empty, Empty) => true
      case (Node(value1, left1, right1), Node(value2, left2, right2)) =>
        if (value1 == value2) areTreesTheSame(right1, right2) && areTreesTheSame(left1, left2)
        else false
      case (_, _) => false
    }
  }

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

  def copyTree[A](tree: BT[A]): BT[A] = {
    tree match {
      case Node(value, left, right) => Node(value, copyTree(left), copyTree(right))
      case Empty => Empty
    }
  }
}
