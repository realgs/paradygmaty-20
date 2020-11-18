import scala.annotation.tailrec
import scala.util.Random

object Functions {
  // Task 1
  def generateTree(depth: Int, valueMin: Int, valueMax: Int): BTree[Int] = {
    if (depth == -1) Empty
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
      BTree(rootDiff(t1, t2).get,
        elementwiseDiff(t1.leftOption.getOrElse(Empty), t2.leftOption.getOrElse(Empty))(rootDiff),
        elementwiseDiff(t1.rightOption.getOrElse(Empty), t2.rightOption.getOrElse(Empty))(rootDiff)
      )
    }
  }

  def deleteDuplicatesDFS(t1: BTree[Int], t2: BTree[Int]): (BTree[Int], BTree[Int]) = {
    if (t1.isLeaf && t2.isLeaf) {
      if (t1.rootOption == t2.rootOption) (Empty, Empty) else (t1, t2)
    }
    else {
      val lhs = deleteDuplicatesDFS(t1.leftOption.get, t2.leftOption.get)
      val rhs = deleteDuplicatesDFS(t1.rightOption.get, t2.rightOption.get)

      if (t1.rootOption == t2.rootOption) {
        (lhs, rhs) match {
          case ((Empty, Empty), (Empty, Empty)) => (Empty, Empty)
          case ((Empty, a), (Empty, b)) => (Empty, BTree(-1, a, b))
          case ((a, Empty), (b, Empty)) => (BTree(-1, a, b), Empty)
          case ((x, a), (y, b)) => (BTree(-1, x, y), BTree(-1, a, b))
        }
      } else {
        (BTree(t1.rootOption.get, lhs._1, rhs._1), BTree(t2.rootOption.get, lhs._2, rhs._2))
      }
    }
  }

  def deleteDuplicatesBFS(t1: BTree[Int], t2: BTree[Int]): (BTree[Any], BTree[Any]) = {
    val bfs1 = t1.toBfsList.reverse
    val bfs2 = t2.toBfsList.reverse
    val depth = t1.depth

    @tailrec
    def auxDelete(xs: List[Int], ys: List[Int])(lowL: List[BTree[Int]], lowR: List[BTree[Int]])(curDepth: Int): (BTree[Int], BTree[Int]) = {
      if (curDepth < 0) return (lowL.head, lowR.head)

      val power = Math.pow(2, curDepth).toInt
      val left = xs.take(power).reverse
      val right = ys.take(power).reverse

      val next = Helper.formTree(left, right)(lowL, lowR)(Nil, Nil)

      auxDelete(Helper.tailOffset(xs)(power), Helper.tailOffset(ys)(power))(next._1, next._2)(curDepth - 1)
    }

    auxDelete(bfs1, bfs2)(List.fill(Math.pow(2, depth + 1).toInt)(Empty), List.fill(Math.pow(2, depth + 1).toInt)(Empty))(depth)
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
