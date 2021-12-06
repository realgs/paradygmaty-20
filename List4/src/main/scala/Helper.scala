import scala.annotation.tailrec

object Helper {
  def tailOffset[A](xs: List[A])(n: Int): List[A] = {
    @tailrec
    def auxTail(xs: List[A], current: Int): List[A] = {
      if (current == n) xs
      else auxTail(xs.tail, current + 1)
    }

    if (xs == Nil) Nil else auxTail(xs, 0)
  }

  @tailrec
  def formTree(upL: List[Int], upR: List[Int])(lowL: List[BTree[Int]], lowR: List[BTree[Int]]) (accL: List[BTree[Int]], accR: List[BTree[Int]]): (List[BTree[Int]], List[BTree[Int]]) = {
    (upL, upR) match {
      case (t1 :: tailL, t2 :: tailR) =>
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
      case (Nil, Nil) => (accL.reverse, accR.reverse)
      case _ => throw new IllegalArgumentException("Tree depth error")
    }
  }

  def isFull[A](t: BTree[A]): Boolean = t match {
    case Empty => true
    case Vertex(_, l, r) =>
      def auxIsFull(left: BTree[A], right: BTree[A]): Boolean = (left, right) match {
        case (Vertex(_, lhs1, rhs1), Vertex(_, lhs2, rhs2)) => auxIsFull(lhs1, rhs1) & auxIsFull(lhs2, rhs2)
        case (Empty, Empty) => true
        case _ => false
      }
      auxIsFull(l, r)
  }
}
