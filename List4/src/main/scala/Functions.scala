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
      BTree(rootDiff(t1, t2).get, elementwiseDiff(t1.leftOption.getOrElse(Empty), t2.leftOption.getOrElse(Empty))(rootDiff),
        elementwiseDiff(t1.rightOption.getOrElse(Empty), t2.rightOption.getOrElse(Empty))(rootDiff))
    }
  }

  def bottomUpDFS[A](t: BTree[A]): LazyList[A] = {
    t match {
      case Vertex(v, l, r) => bottomUpDFS(l) #::: v #:: bottomUpDFS(r)
      case Empty => LazyList()
    }
  }

  def deleteDuplicatesBFS(t1: BTree[Int], t2: BTree[Int]): (BTree[Int], BTree[Int]) = {
    if (t1.isLeaf && t2.isLeaf) {
      if (t1.rootOption == t2.rootOption) (Empty, Empty) else (t1, t2)
    }
    else {
      val lhs = deleteDuplicatesBFS(t1.leftOption.get, t2.leftOption.get)
      val rhs = deleteDuplicatesBFS(t1.rightOption.get, t2.rightOption.get)

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

  /*
  def deleteDuplicatesBFS(t1: BTree[Int], t2: BTree[Int]): Unit = {
    // Not exception safe
    val depth = Helper.treeDepth(t1)

    val bfs1 = t1.toBfsList.reverse
    val bfs2 = t2.toBfsList.reverse
    /*
        def aux2(bfs1: List[Int], bfs2: List[Int], even: Boolean, count: Int, depth: Int): (List[Int], List[Int]) = {
          if (count * 2 > depth) aux2(bfs1, bfs2, even = true, 0, depth - 1)
          else {
            (bfs1, bfs2) match {
              case (h1 :: t1, h2 :: t2) => if (h1 == h2) aux2(t1, t2, !even, count + 1, depth) else ???
            }
          }
        }



        def aux(q1: List[BTree[Int]], q2: List[BTree[Int]]) = {
          (q1, q2) match {
            case (Vertex(x, lhs1, rhs1) :: t1, Vertex(y, lhs2, rhs2) :: t2) => {
              if (x == y) {
                (Vertex(None, lhs1, lhs2), Vertex(None, rhs1, rhs2))
              }

            }
          }
        }

     */
  }

  def auxBFS(t: BTree[Int], compareTo: BTree[Int]): BTree[Int] = {
    if (t.isLeaf & compareTo.isLeaf) {
      if (t.rootOption == compareTo.rootOption) Empty else t
    }
    else {
      if (t.rootOption == compareTo.rootOption) {
        val lhs = auxBFS(t.getLeft.get, compareTo.getLeft.get)
        val rhs = auxBFS(t.getRight.get, compareTo.getRight.get)

        () =>


      }
    }
  }

  def aux1(t1: BTree[Int], t2: BTree[Int]): (BTree[Int], BTree[Int]) = {
    if (t1.isLeaf && t2.isLeaf) {
      if (t1.rootOption == t2.rootOption) (Empty, Empty) else (t1, t2)
    }
    else {
      if (t1.rootOption == t2.rootOption) {
        val lhs = () => aux1(t1.getLeft.get, t2.getLeft.get)
        val rhs = () => aux1(t1.getRight.get, t2.getRight.get)

        (lhs(), rhs()) match {
          case ((Empty, Empty), (Empty, Empty)) => (Empty, Empty)
          case ((Empty, a), (Empty, b)) => (Empty, BTree(-1, a, b))
          case ((a, Empty), (b, Empty)) => (BTree(-1, a, b), Empty)
          case ((x, a), (y, b)) => (BTree(-1, x, y), BTree(-1, a, b))
        }
      } else {
        aux1(t1, t2)
      }
    }
  }
  */

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
