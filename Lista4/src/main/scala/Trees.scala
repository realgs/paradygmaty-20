import scala.util.Random

object Trees {

  //Definicja drzewa
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //Zad 1 (3 pkt)
  def generateTree (height: Int, lowerBoundary: Int, upperBoundary: Int): BT[Int] = {
    if(height < 0 || lowerBoundary > upperBoundary || lowerBoundary < 0) throw new Exception("Illegal argument value!")
    else if(height == 0) Empty
    else Node(Random.between(lowerBoundary, upperBoundary + 1), generateTree(height - 1, lowerBoundary, upperBoundary), generateTree(height - 1, lowerBoundary, upperBoundary))
  }

  //funkcje potrzebne do sprawdzenia drzewa
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

  //Zad 2 (3 pkt)
  def subtractInTrees (tree1: BT[Int], tree2: BT[Int]): BT[Int] = {
    if(getHeight(tree1) != getHeight(tree2) || !isTreeFull(tree1) || !isTreeFull(tree2))
      throw new Exception("Trees not full or equal in height!")
    else (tree1, tree2) match {
      case (Node(val1, left1, right1), Node(val2, left2, right2)) => Node(val1 - val2, subtractInTrees(left1, left2), subtractInTrees(right1, right2))
      case _ => Empty
    }
  }

  //Zad 3 (1 pkt dfs, 3 pkt bfs)
  def removeSameValuesDFS (tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if(getHeight(tree1) != getHeight(tree2) || !isTreeFull(tree1) || !isTreeFull(tree2))
      throw new Exception("Trees not full or equal in height!")
    else (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(val1, left1, right1), Node(val2, left2, right2)) =>
        val (subLeft1, subLeft2) = removeSameValuesDFS(left1, left2)
        val (subRight1, subRight2) = removeSameValuesDFS(right1, right2)
        if(val1 == val2 && subLeft1 == Empty && subRight1 == Empty) (Empty, Empty)
        else if(val1 == val2) (Node(-1, subLeft1, subRight1), Node(-1, subLeft2, subRight2))
        else (Node(val1, subLeft1, subRight1), Node(val2, subLeft2, subRight2))
    }
  }

}
