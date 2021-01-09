import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Trees {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def fullTree(height: Int, lower: Int, upper: Int): BT[Int] = {
    val r = new scala.util.Random()

    def generateTree(h: Int): BT[Int] = {
      h match {
        case 0 => Empty;
        case _ => Node(r.between(lower, upper), generateTree(h - 1), generateTree(h - 1));
      }
    }

    if (lower < 0 || lower > upper || height < 0) Empty
    else generateTree(height)
  }

  def removeRepetition(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Empty, _) | (_, Empty) => throw new Exception("Wrong input.Trees have different height")
      case (Node(v1, leftTree1, rightTree1), Node(v2, leftTree2, rightTree2)) =>
        val (left1, left2) = removeRepetition(leftTree1, leftTree2)
        val (right1, right2) = removeRepetition(rightTree1, rightTree2)
        if (v1 == v2) {
          if (left1 == Empty && right2 == Empty) return (Empty, Empty)
          else return (Node(-1, left1, right1), Node(-1, left2, right2))
        }
        else return (Node(v1, left1, right1), Node(v2, left2, right2))
    }
  }

  def removeRepetitionFuture(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Empty, _) | (_, Empty) => throw new Exception("Wrong input.Trees have different height")
      case (Node(v1, leftTree1, rightTree1), Node(v2, leftTree2, rightTree2)) =>
        val future1 = Future(removeRepetition(leftTree1, leftTree2))
        val future2 = Future(removeRepetition(rightTree1, rightTree2))
        val (left1, left2) = Await.result(future1, 1000.seconds)
        val (right1, right2) = Await.result(future2, 1000.seconds)
        if (v1 == v2) {
          if (left1 == Empty && right2 == Empty) return (Empty, Empty)
          else return (Node(-1, left1, right1), Node(-1, left2, right2))
        }
        else return (Node(v1, left1, right1), Node(v2, left2, right2))
    }
  }

  def removeRepetitionParallel(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Empty, _) | (_, Empty) => throw new Exception("Wrong input.Trees have different height")
      case (Node(v1, leftTree1, rightTree1), Node(v2, leftTree2, rightTree2)) =>
        val (left, right) = parallel(removeRepetition(leftTree1, leftTree2), removeRepetition(rightTree1, rightTree2))
        if (v1 == v2) {
          if (left._1 == Empty && right._2 == Empty) return (Empty, Empty)
          else return (Node(-1, left._1, right._1), Node(-1, left._2, right._2))
        }
        else return (Node(v1, left._1, right._1), Node(v2, left._2, right._2))
    }
  }
}
