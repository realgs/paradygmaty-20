import scala.annotation.tailrec

object Helper {
  // TODO Change this
  def treeDepth[A](t: BTree[A]): Int = t match {
    case Empty => 0
    case Vertex(_, l, r) => 1 + (treeDepth(l) max treeDepth(r))
  }

  def splitListPowers[A](xs: List[A], base: Int): List[List[A]] = {
    if (base <= 0) Nil

    xs match {
      case h :: t => List(h) :: splitListPowers(t, base)
    }
  }

  def tailOffset[A](xs: List[A])(n: Int): List[A] = {
    @tailrec
    def auxTail(xs: List[A], current: Int): List[A] = {
      if (current == n) xs
      else auxTail(xs.tail, current + 1)
    }

    auxTail(xs, 0)
  }

  @tailrec
  def formTree[Int](upL: List[Int], upR: List[Int])(lowL: List[BTree[Any]], lowR: List[BTree[Any]])(accL: List[BTree[Any]], accR: List[BTree[Any]]): (List[BTree[Any]], List[BTree[Any]]) = {
    (upL, upR) match {
      case (t1 :: tailL, t2 :: tailR) => {
        val List(leftSub1, rightSub1) = lowL.take(2)
        val List(leftSub2, rightSub2) = lowR.take(2)

        if (t1 == t2) {
          (leftSub1, rightSub1, leftSub2, rightSub2) match {
            case (Empty, Empty, Empty, Empty) => formTree(tailL, tailR)(tailOffset(lowL)(2), tailOffset(lowR)(2))(Empty :: accL, Empty :: accR)
            case (l1, Empty, l2, Empty) => formTree(tailL, tailR)(tailOffset(lowL)(2), tailOffset(lowR)(2))(Vertex(-1, l1, Empty) :: accL, Vertex(-1, l2, Empty) :: accR)
            case (Empty, r1, Empty, r2) => formTree(tailL, tailR)(tailOffset(lowL)(2), tailOffset(lowR)(2))(Vertex(-1, Empty, r1) :: accL, Vertex(-1, Empty, r2) :: accR)
            case _ => formTree(tailL, tailR)(tailOffset(lowL)(2), tailOffset(lowR)(2))(Vertex(-1, leftSub1, rightSub1) :: accL, Vertex(-1, leftSub2, rightSub2) :: accR)
          }
        }
        else {
          formTree(tailL, tailR)(tailOffset(lowL)(2), tailOffset(lowR)(2))(Vertex(t1, leftSub1, rightSub1) :: accL, Vertex(t2, leftSub2, rightSub2) :: accR)
        }

      }
      case (Nil, Nil) => (accL.reverse, accR.reverse)
      case _ => throw new IllegalArgumentException("Tree depth error")
    }
  }
}
