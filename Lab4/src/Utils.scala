import scala.util.Random

object Utils {

  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  def randomInt(range: (Int, Int)): Int =
    Random.nextInt(range._2 - range._1 + 1) + range._1

  def max(x: Int, y: Int): Int =
    if(x > y) x else y

  def height[A](tree: BinaryTree[A]): Int = tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + max(height(left), height(right))
  }

  def countNodes[A](tree: BinaryTree[A]): Int = tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + countNodes(left) + countNodes(right)
  }

  def isFullTree[A](tree: BinaryTree[A]): Boolean =
    countNodes(tree) == Math.pow(2, height(tree)) - 1

  def intLazyListGenerator(): LazyList[Int] = {
    def generator(i: Int): LazyList[Int] =
      i #:: generator(i + 1)
    generator(1)
  }

}
