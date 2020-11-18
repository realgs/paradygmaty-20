import scala.util.Random

object Trees {

  //Definicja drzewa
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //Zad 1 (3 pkt)
  def generateTree(height: Int, lowerBoundary: Int, upperBoundary: Int): BT[Int] = {
    if(height < 0 || lowerBoundary > upperBoundary || lowerBoundary < 0) throw new Exception("Illegal argument value!")
    else if(height == 0) Node(Random.between(lowerBoundary, upperBoundary), Empty, Empty)
    else Node(Random.between(lowerBoundary, upperBoundary), generateTree(height - 1, lowerBoundary, upperBoundary), generateTree(height - 1, lowerBoundary, upperBoundary))
  }

  //funkcje potrzebne do sprawdzenia drzewa
  def getHeight[A](tree: BT[A]): Int = {
    def height(inTree: BT[A]): Int = {
      inTree match {
        case Empty => 0
        case Node(_, Empty, Empty) => 0
        case Node(_, subLeft, subRight) => 1 + Math.max(height(subLeft), height(subRight))
      }
    }
    if (tree == Empty) -1
    else height(tree)
  }

  def isTreeFull[A] (tree: BT[A]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, subLeft, Empty) => if (subLeft == Empty) true else false
      case Node(_, Empty, subRight) => if (subRight == Empty) true else false
      case Node(_, subLeft, subRight) => isTreeFull(subLeft) && isTreeFull(subRight)
    }
  }

}
