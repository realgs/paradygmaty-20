package Lista4

object Lista4 extends App{
  // Tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


  // Exceptions
  class WrongValueOfArguments extends Exception


  // Test Methods
  def isTreeFull(tree: BT[Int]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, l, r) =>
        def innerIsTreeFull(leftTree: BT[Int], rightTree: BT[Int]): Boolean ={
          (leftTree, rightTree) match {
            case(Empty, Empty) => true
            case(Empty, Node(_, _, _)) => false
            case(Node(_, _, _), Empty) => false
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeFull(l1, r1) && innerIsTreeFull(l2, r2)
          }
      }
        innerIsTreeFull(l, r)
    }
  }

  def isTreeDepthRight(tree: BT[Int], expectedLevel: Int): Boolean = {
    tree match {
      case Empty => false
      case Node(_, l, r) =>
        def innerIsTreeDepthRight(leftTree: BT[Int], rightTree: BT[Int], level: Int): Boolean ={
          (leftTree, rightTree) match {
            case(Empty, Empty) => expectedLevel == 0
            case(Empty, Node(_, _, _)) => false
            case(Node(_, _, _), Empty) => false
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeDepthRight(l1, r1, level-1) == innerIsTreeDepthRight(l2, r2, level-1)
          }
        }
        innerIsTreeDepthRight(l, r, expectedLevel)
    }
  }


  // zadanie 1 (3pkt)
  // dla amountOfLevels == 0 zwraca drzewo z samym korzeniem z wartoscia - potomkami korzenia sa Empty
  // dla amountOfLevels < 0 rzuca blad
  def createTree(amountOfLevels: Int, leftInterval: Int, rightInterval: Int): BT[Int] = {
    if (amountOfLevels < 0 || rightInterval < leftInterval) throw new WrongValueOfArguments
    val randomInt = scala.util.Random
    def innerCreateTree(levels: Int): BT[Int] = {
      if(levels == (-1)) Empty
      else Node(randomInt.nextInt(rightInterval-leftInterval+1)+leftInterval, innerCreateTree(levels-1), innerCreateTree(levels-1))
    }
    innerCreateTree(amountOfLevels)
  }
}
