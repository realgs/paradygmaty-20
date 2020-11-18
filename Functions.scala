import scala.util.Random

class Functions {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val r = new Random()


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

  def checkDepth(tree: BT[Int]): Int = {
    def helper(tree: BT[Int], depth: Int): Int =
      tree match {
        case Empty => depth
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

  private def diff(first: BT[Int], second: BT[Int]): BT[Int] =
    (first, second) match {
      case (Empty, Empty) => Empty
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node(v1 - v2, diff(l1, l2), diff(r1, r2))
    }

  //zadanie 3(1+3pkt)
  def deleteDuplicatesDFS(first: BT[Int], second: BT[Int]): (BT[Int], BT[Int]) = {
    if (checkDepth(first) == checkDepth(second)) DFS(first, second)
    else throw new Exception("Depths must be equal")
  }

  private def DFS(first: BT[Int], second: BT[Int]): (BT[Int], BT[Int]) = {
    (first, second) match {
      case (Node(v1, Empty, Empty), Node(v2, Empty, Empty)) => if (v1 == v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => {
        val (l1Tree, l2Tree) = DFS(l1, l2)
        val (r1Tree, r2Tree) = DFS(r1, r2)
        (l1Tree, l2Tree, r1Tree, r2Tree) match {
          case (Empty, Empty, Empty, Empty) => if (v1 == v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
          case (l1, l2, r1, r2) => if (v1 == v2) (Node(-1, l1, r1), Node(-1, l2, r2)) else (Node(v1, l1, r1), Node(v2, l2, r2))
        }
      }
    }
  }


  def deleteDuplicatesBFS(firstTree: BT[Int], secondTree: BT[Int]): (BT[Int], BT[Int]) = {
    if (checkDepth(firstTree) == checkDepth(secondTree) & isTreeFull(firstTree) & isTreeFull(secondTree)) BFS(firstTree, secondTree)
    else throw new Exception("Depths must be equal")
  }

  def BFS(first: BT[Int], second: BT[Int]): (BT[Int], BT[Int]) = {
    (first, second) match {
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => {
        val areLeftSubTreesEqual = isTreeBFSEqual(l1, l2)
        val areRightSubTreesEqual = isTreeBFSEqual(r1, r2)
        (areLeftSubTreesEqual, areRightSubTreesEqual) match {
          case (true, true) => if (v1 == v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
          case (false, false) =>
            val (ll1, lr2) = BFS(l1, l2)
            val (rl1, rl2) = BFS(r1, r2)
            (Node(if (v1 == v2) -1 else v1, ll1, rl1), Node(if (v1 == v2) -1 else v2, lr2, rl2))
          case (true, false) =>
            val (lTree, rTree) = BFS(r1, r2)
            (Node(if (v1 == v2) -1 else v1, Empty, lTree), Node(if (v1 == v2) -1 else v2, Empty, rTree))
          case (false, true) =>
            val (lTree, rTree) = BFS(l1, l2)
            (Node(if (v1 == v2) -1 else v1, lTree, Empty), Node(if (v1 == v2) -1 else v2, rTree, Empty))
        }
      }

    }
  }

  def isTreeBFSEqual[A](first: BT[A], second: BT[A]): Boolean = {
    def breadthInner[A](firstQueue: List[BT[A]], secondQueue: List[BT[A]]): Boolean =
      (firstQueue, secondQueue) match {
        case (Nil, Nil) => true
        case (Empty :: t1, Empty :: t2) => breadthInner(t1, t2)
        case (Node(v1, l1, r1) :: t1, Node(v2, l2, r2) :: t2) =>
          (v1 == v2) & breadthInner(t1 ::: List(l1, r1), t2 ::: List(l2, r2))
        case (_, _) => false
      }

    breadthInner(List(first), List(second))
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

  def ldzialanieD(first: LazyList[Double], second: LazyList[Double], operator: Char): LazyList[Double] = {
    operator match {
      case '+' => ldzialanieHelper((a: Double, b: Double) => a + b)(first, second)
      case '-' => ldzialanieHelper((a: Double, b: Double) => a - b)(first, second)
      case '*' => ldzialanieHelper((a: Double, b: Double) => a * b)(first, second)
      case '/' => ldzialanieHelper((a: Double, b: Double) => a / b)(first, second)
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
