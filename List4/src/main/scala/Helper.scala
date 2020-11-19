import javax.swing.plaf.basic.BasicFormattedTextFieldUI

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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
  def formTree(upL: List[Int], upR: List[Int])(lowL: List[BTree[Int]], lowR: List[BTree[Int]])(accL: List[BTree[Int]], accR: List[BTree[Int]]): (List[BTree[Int]], List[BTree[Int]]) = {
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

  def lazyTreeBuilder[A](t: BTree[A]): BTree[() => A] = {
    t.rootOption match {
      case None => Empty
      case Some(x) => BTree(() => x, lazyTreeBuilder(t.leftOption.getOrElse(Empty)), lazyTreeBuilder(t.rightOption.getOrElse(Empty)))
    }
  }

  def bfsBacktrackDiscover[A](t1: BTree[A], t2: BTree[A])(func: (A, A) => (A, A)): (() => BTree[A], () => BTree[A]) = {
    def auxBFS(q1: Queue[BTree[A]], q2: Queue[BTree[A]])(accL: () => BTree[A], accR: () => BTree[A]): Unit = {
      if (q1.isEmpty && q2.isEmpty) (accL, accR)
      else {
        (q1.dequeue, q2.dequeue) match {
          case ((Vertex(x, lhs1, rhs1), q1m), (Vertex(y, lhs2, rhs2), q2m)) => {
            auxBFS(q1m.enqueue(lhs1).enqueue(rhs1), q2m.enqueue(lhs2).enqueue(rhs2))(accL(), () => Vertex(y, lhs2, rhs2))
            val roots = func(x, y)
          }
          case ((Empty, q1m), (Empty, q2m)) => {
            auxBFS(q1m, q2m)(accL, accR)
          }
          case _ => throw new IllegalArgumentException("Tree structure mismatch")
        }
      }
    }
    auxBFS(Queue(t1), Queue(t2))(() => Empty, () => Empty)
  }
}
