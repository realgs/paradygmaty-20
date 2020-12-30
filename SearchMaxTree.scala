package List6

import List6.Parallel.parallel
import scala.annotation.tailrec

object SearchMaxTree {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def searchMax(tree: BT[Int]): Int = {
    @tailrec
    def innerSearchMax(queue: List[BT[Int]], max: Int): Int = {
      queue match {
        case Empty :: _ => max
        case Node(v, l, r) :: _ => innerSearchMax(queue.tail ::: List(l,r), if(v > max) v else max)
      }
    }
    innerSearchMax(List(tree), Int.MinValue)
  }

  def parSearchMax(tree: BT[Int]): Int = {
    def rootSubtrees(queue: List[BT[Int]]): (BT[Int], BT[Int], Int) = {
      queue match {
        case Empty :: _ => (Empty, Empty, Int.MinValue)
        case Node(v, l, r) :: _ => (l, r, v)
      }
    }
    val subtreesAndMax = rootSubtrees(List(tree))
    val (left, right) = parallel(searchMax(subtreesAndMax._1), searchMax(subtreesAndMax._2))
    Math.max(Math.max(left, right), subtreesAndMax._3)
  }

}
