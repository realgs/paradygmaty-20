import scala.util.Random

object Functions {
  Random.setSeed(0)

  // Task 1
  def generateTree(depth: Int, valueMin: Int, valueMax: Int): BTree[Int] = {
    if (depth == 0) Empty
    else {
      BTree(Random.between(valueMin, valueMax), generateTree(depth - 1, valueMin, valueMax),
        generateTree(depth - 1, valueMin, valueMax))
    }
  }

  def intRootDiff(lhs: BTree[Int], rhs: BTree[Int]): Option[Int] = {
    (lhs.rootOption, rhs.rootOption) match {
      case (Some(x), Some(y)) => Some(x - y)
      case _ => None
    }
  }

  // Task 2
  def elementwiseDiff(t1: BTree[Int], t2: BTree[Int])(rootDiff: (BTree[Int], BTree[Int]) => Option[Int]): BTree[Int] = {
    if (rootDiff(t1, t2).isEmpty) Empty
    else {
      BTree(rootDiff(t1, t2).get, elementwiseDiff(t1.getLeft.getOrElse(Empty), t2.getLeft.getOrElse(Empty))(rootDiff),
        elementwiseDiff(t1.getRight.getOrElse(Empty), t2.getRight.getOrElse(Empty))(rootDiff))
    }
  }

  def bottomUpDFS[A](t: BTree[A]): LazyList[A] = {
    t match {
      case Vertex(v, l, r) => bottomUpDFS(l) #::: v #:: bottomUpDFS(r)
      case Empty => LazyList()
    }
  }

  def deleteDuplicates(lhs: BTree[Int], rhs: BTree[Int]): (BTree[Int], BTree[Int]) = {
    if (lhs.isLeaf && rhs.isLeaf) (Empty, Empty)
    else {
      // TODO: This is severely flawed
      if (lhs.rootOption == rhs.rootOption) {
        (lhs.getLeft.getOrElse(Empty).rootOption, lhs.getRight.getOrElse(Empty).rootOption, rhs.getLeft.getOrElse(Empty).rootOption, rhs.getRight.getOrElse(Empty).rootOption) match {
          case (Some(x), None, Some(y), None) => (Vertex(-1, Vertex(x), Empty), Vertex(-1, Vertex(y), Empty))
          case (None, Some(x), None, Some(y)) => (Vertex(-1, Empty, Vertex(x)), Vertex(-1, Empty, Vertex(y)))
          case _ => ???
        }
      } else {
        (lhs, rhs)
      }
    }
  }

  // Task 4
  def eachNElement[A](lxs: LazyList[A], n: Int): LazyList[A] = {
    def auxChooseNTh(lxs: LazyList[A], step: Int): LazyList[A] = {
      (step, lxs) match {
        case (k, h #:: t) => if (k == n) h #:: auxChooseNTh(t, 1) else auxChooseNTh(t, k + 1)
        case _ => LazyList()
      }
    }

    if (n >= 1) auxChooseNTh(lxs, n) else throw new IllegalArgumentException("Parameter n out of bounds")
  }

  // Task 5
  def apply[A](lxs: LazyList[A], lys: LazyList[A])(func: (A, A) => A): LazyList[A] = {
    (lxs, lys) match {
      case (hx #:: tx, hy #:: ty) => func(hx, hy) #:: apply(tx, ty)(func)
      case (lst, _) => lst
      case (_, lst) => lst
    }
  }
}
