import scala.collection.View.Empty
import scala.util.Random

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

class Functions {
  val r = new Random()
  //for testing purposes
  val firstTestTree = Node(10,Node(12,Node(6,Empty,Empty),Node(13,Empty,Empty)),Node(6,Node(1,Empty,Empty),Node(12,Empty,Empty)))
  val secondTestTree = Node(2,Node(7,Node(8,Empty,Empty),Node(7,Empty,Empty)),Node(2,Node(9,Empty,Empty),Node(9,Empty,Empty)))
  val resultTree = Node(8,Node(5,Node(-2,Empty,Empty),Node(6,Empty,Empty)),Node(4,Node(-8,Empty,Empty),Node(3,Empty,Empty)))

  //zadanie 1 (3pkt)
  def generateTree(depth: Int, x: Int, y: Int): BT[Int] = {
    if (depth < 1) throw new Exception("Depth must be higher than 0!")
    else if (y < x || y < 0 || x < 0) throw new Exception("Incorrect range!")
    else getNode(depth, x, y)
  }

  private def getNode(depth: Int, x: Int, y: Int): BT[Int] = {
    depth match {
      case 0 => Empty
      case i => Node(r.nextInt(y - x) + x, getNode(i - 1, x, y), getNode(i - 1, x, y))

    }
  }

  def checkDepth(tree: BT[Int]): Int = {
    def helper(tree: BT[Int], depth: Int): Int =
      tree match {
        case Empty => depth
        case Node(_, left, _) => helper(left, depth + 1)
      }

    helper(tree, 0)
  }

  //for testing purposes
  def isTreeFull(tree: BT[Int]): Boolean = {
    def checkSubtree(subtree: BT[Int]): Boolean =
      subtree match {
        case Empty => true
        case Node(_, Empty, Empty) => true
        case Node(_, Empty, _) => false
        case Node(_, _, Empty) => false
        case Node(_, left, right) => (checkDepth(left) == checkDepth(right)) & checkSubtree(left) & checkSubtree(right)
      }
    checkSubtree(tree)
  }

  //zadanie 2 (3pkt)
  def diffTrees(first: BT[Int], second: BT[Int]): BT[Int] = {
    if (checkDepth(first) == checkDepth(second)) {
      diff(first, second)
    }
    else throw new Exception("Invalid arguments")
  }

  private def diff(first: BT[Int], second: BT[Int]): BT[Int] =
    (first, second) match {
      case (Empty, Empty) => Empty
      case (Node(firstElem, l1, r1), Node(secondElem, l2, r2)) => Node(firstElem - secondElem, diff(l1, l2), diff(r1, r2))
    }

  //zadanie 4 (5pkt)
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
    if (n < 1 || m < 0) throw new Exception("Invalid arguments")

    def eachNElementIter[A](list: LazyList[A], counter: Int): LazyList[A] = {
      if (counter == m) LazyList()
      else list match {
        case value #:: tail =>
          if (counter % n == 0) value #:: eachNElementIter(tail, counter + 1)
          else eachNElementIter(tail, counter + 1)
        case _ => LazyList()
      }
    }

    eachNElementIter(list, 0)
  }

  //zadanie 5 (5pkt)
  def ldzialanie(first: LazyList[Int], second: LazyList[Int], operator: Char): LazyList[Int] = {
    operator match {
      case '+' => ldzialanieHelper((a: Int, b: Int) => a + b)(first, second)
      case '-' => ldzialanieHelper((a: Int, b: Int) => a - b)(first, second)
      case '*' => ldzialanieHelper((a: Int, b: Int) => a * b)(first, second)
      case '/' => ldzialanieHelper((a: Int, b: Int) => a / b)(first, second)
      case _ => throw new Exception("Incorrect operator")
    }
  }

  private def ldzialanieHelper[A](operation: (A, A) => A)(first: LazyList[A], second: LazyList[A]): LazyList[A] = {
    (first, second) match {
      case (val1 #:: tailFirst, val2 #:: tailSecond) => operation(val1, val2) #:: ldzialanieHelper(operation)(tailFirst, tailSecond)
      case (first, LazyList()) => first
      case (LazyList(), second) => second
      case (_, _) => LazyList()
    }
  }

}
