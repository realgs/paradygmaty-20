package scala.helper

import scala.trees.{BT, Empty, Node}

object HelperFunctions {
  def treeTest(tree: BT[Int], start: Int, end: Int): Boolean = {
    tree match {
      case Empty => true
      case Node(value, left, right) => if (value >= start && value <= end) treeTest(left, start, end) && treeTest(right, start, end) else false
    }
  }

  def depth[A](tree: BT[A]): Int = {
    def helper(bt: BT[A], result: Int): Int =
      bt match {
        case Empty => 0
        case Node(_, left, right) => if (helper(left, result) > helper(right, result)) helper(left, result) + 1 else helper(right, result) + 1
      }
    helper(tree, 0)
  }

}
