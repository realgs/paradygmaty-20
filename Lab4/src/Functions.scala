import scala.util.Random

object Functions {

  sealed trait BinaryTree[+A]
  case object Empty extends BinaryTree[Nothing]
  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  // Zadanie 1 (3 pkt)
  def randomInt(range: (Int, Int)): Int =
    Random.nextInt(range._2 - range._1 + 1) + range._1

  def generateBinaryTree(n: Int, range: (Int, Int)): BinaryTree[Int] = {
    def generate(k: Int): BinaryTree[Int] =
      if(k == 0) Empty
      else Node(randomInt(range), generate(k - 1), generate(k - 1))

    if(n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if(range._1 <= 0) throw new IllegalArgumentException("range must be positive")
    else if(range._1 > range._2) throw new IllegalArgumentException("invalid range")
    else generate(n)
  }

  // Zadanie 2 (3 pkt)
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

  def subtractTrees(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): BinaryTree[Int] =
    if(!isFullTree(tree1) || !isFullTree(tree2)) throw new IllegalArgumentException("both trees must be full")
    else if(height(tree1) != height(tree2)) throw new IllegalArgumentException("both trees must be the same height")
    else (tree1, tree2) match {
      case (Empty, Empty) => Empty
      case (Node(v1, left1, right1), Node(v2, left2, right2)) => Node(v1 - v2, subtractTrees(left1, left2), subtractTrees(right1, right2))
    }

  // Zadanie 3 (4 pkt)
  def deleteDuplicatesDFS(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) = {
    def delete(tree1: BinaryTree[Int], tree2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) =
      (tree1, tree2) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(v1, left1, right1), Node(v2, left2, right2)) =>

          val (newLeft1, newLeft2) = delete(left1, left2)
          val (newRight1, newRight2) = delete(right1, right2)

          if (v1 == v2 && newLeft1 == Empty && newRight1 == Empty) (Empty, Empty)
          else if (v1 == v2) (Node(-1, newLeft1, newRight1), Node(-1, newLeft2, newRight2))
          else (Node(v1, newLeft1, newRight1), Node(v2, newLeft2, newRight2))
      }

    if(!isFullTree(tree1) || !isFullTree(tree2)) throw new IllegalArgumentException("both trees must be full")
    else if(height(tree1) != height(tree2)) throw new IllegalArgumentException("both trees must be the same height")
    else delete(tree1, tree2)
  }

  // Zadanie 4 (5 pkt)
  def intLazyListGenerator(): LazyList[Int] = {
    def generator(i: Int): LazyList[Int] =
      i #:: generator(i + 1)
    generator(1)
  }

  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementHelper(list: LazyList[A], i: Int): LazyList[A] =
      if(list.isEmpty || i >= m) LazyList()
      else if(i % n == 0) list.head #:: eachNElementHelper(list.tail, i + 1)
      else eachNElementHelper(list.tail, i + 1)

    if(n < 1) throw new IllegalArgumentException("n must be positive")
    else if(m < 1) throw new IllegalArgumentException("m must be positive")
    eachNElementHelper(list, 0)
  }

  // Zadanie 5 (5 pkt)
  def ldzialanie(list1: LazyList[Double], list2: LazyList[Double], operation: (Double, Double) => Double): LazyList[Double] =
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (h1 #:: t1, LazyList()) => operation(h1, 0) #:: ldzialanie(t1, LazyList(), operation)
      case (LazyList(), h2 #:: t2) => operation(0, h2) #:: ldzialanie(LazyList(), t2, operation)
      case (h1 #:: t1, h2 #:: t2) => operation(h1, h2) #:: ldzialanie(t1, t2, operation)
    }
}
