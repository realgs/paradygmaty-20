import scala.util.Random

class Functions {

  sealed trait BT[A]

  case class Leaf[A]() extends BT[A]

  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val r = new Random()

  //zadanie 1 (3pkt)
  def generateTree(depth: Int, x: Int, y: Int): BT[Int] = {
    if (depth < 1) throw new Exception("Depth must be higher than 0!")
    else if (y > x || y < 0 || x < 0) throw new Exception("Incorrect range!")
    else getNode(depth, x, y)
  }

  private def getNode(depth: Int, x: Int, y: Int): BT[Int] = {
    depth match {
      case 0 => Leaf()
      case i => Node(r.nextInt(y - x) + x, getNode(i - 1, x, y), getNode(i - 1, x, y))

    }
  }

  private def checkDepth(tree: BT[Int]): Int = {
    def helper(tree: BT[Int], depth: Int): Int =
      tree match {
        case Leaf() => depth
        case Node(_, left, _) => helper(left, depth + 1)
      }

    helper(tree, 0)
  }

  //zadanie 2 (3pkt)
  def diffTrees(first: BT[Int], second: BT[Int]): BT[Int] = {
    if (checkDepth(first) == checkDepth(second)) {
      diff(first, second)
    }
    else throw new Exception("Invalid arguments")
  }

  def diff(first: BT[Int], second: BT[Int]): BT[Int] =
    (first, second) match {
      case (Leaf(), Leaf()) => Leaf()
      case (Node(firstElem, l1, r1), Node(secondElem, l2, r2)) => Node(firstElem - secondElem,diff(l1, l2), diff(r1, r2))
    }


}
