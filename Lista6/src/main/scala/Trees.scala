import ParallelUtilities.parallel
import scala.util.Random

object Trees {
  val MAX_THREADS = 4 //ograniczenie wynikające z domniemanej liczby wątków, które obsługuje równocześnie mój procesor

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  def generateTree (height: Int, lowerBoundary: Int, upperBoundary: Int): BT[Int] = {
    if(height < 0 || lowerBoundary > upperBoundary || lowerBoundary < 0) throw new Exception("Illegal argument value!")
    else if(height == 0) Empty
    else Node(lowerBoundary + Random.nextInt(upperBoundary - lowerBoundary + 1), generateTree(height - 1, lowerBoundary, upperBoundary), generateTree(height - 1, lowerBoundary, upperBoundary))
  }

  def getHeight[A] (tree: BT[A]): Int = {
    tree match {
      case Empty => 0
      case Node(_, subLeft, subRight) => 1 + Math.max(getHeight(subLeft), getHeight(subRight))
    }
  }

  def isTreeFull[A] (tree: BT[A]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, subLeft, Empty) => if (subLeft == Empty) true else false
      case Node(_, Empty, subRight) => if (subRight == Empty) true else false
      case Node(_, subLeft, subRight) => isTreeFull(subLeft) && isTreeFull(subRight)
    }
  }

  def treeToList[A] (tree: BT[A]): List[A] =
    tree match {
      case Node(value, left, right) => treeToList(left) ::: (value :: treeToList(right))
      case Empty => Nil
    }

  private def parTreeToList[A] (tree: BT[A], threads: Int): List[A] =
    tree match {
      case Node(value, left, right) =>
        if(threads < MAX_THREADS){
          val lists = parallel(parTreeToList(left, threads * 2), parTreeToList(right, threads * 2))
          lists._1 ::: (value :: lists._2)
        }
        else{
          treeToList(left) ::: (value :: treeToList(right))
        }
      case Empty => Nil
    }

  def parallelTreeToList[A](tree: BT[A]): List[A] = {
    parTreeToList(tree, 1)
  }
}
