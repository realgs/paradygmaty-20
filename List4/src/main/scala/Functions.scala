import scala.collection.immutable.Queue
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

  def deleteDuplicates(t1: BTree[Int], t2: BTree[Int]): (BTree[Int], BTree[Int]) = {
    if (t1.isLeaf && t2.isLeaf) {
      if (t1.rootOption == t2.rootOption) (Empty, Empty) else (t1, t2)
    }
    else {
      if (t1.rootOption == t2.rootOption) {
        val lhs = deleteDuplicates(t1.getLeft.get, t2.getLeft.get)
        val rhs = deleteDuplicates(t1.getRight.get, t2.getRight.get)

        (lhs, rhs) match {
          case ((Empty, Empty), (Empty, Empty)) => (Empty, Empty)
          case ((Empty, a), (Empty, b)) => (Empty, BTree(-1, a, b))
          case ((a, Empty), (b, Empty)) => (BTree(-1, a, b), Empty)
          case ((x, a), (y, b)) => (BTree(-1, x, y), BTree(-1, a, b))
        }
      } else {
        deleteDuplicates(t1, t2)
      }
    }
  }

  def toBFSList[A](tree: BTree[A], reverse: Boolean): List[A] = {
    ???
  }

  def deleteDuplicatesBFS(t1: BTree[Int], t2: BTree[Int]): (BTree[Int], BTree[Int]) = {
    ???
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
