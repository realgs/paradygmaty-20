package Lista4

object Lista4 extends App{
  // Tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  // Exceptions
  class WrongValueOfArguments extends Exception

  // Additional methods



  // zadanie 1 (3pkt)
  def createTree(amountOfLevels: Int, leftInterval: Int, rightInterval: Int): BT[Int] = {
    if (amountOfLevels<0 || rightInterval<leftInterval) throw new WrongValueOfArguments
    val randomInt = scala.util.Random
    def innerCreateTree(levels: Int): BT[Int] = {
      if(levels == (-1)) Empty
      else Node(randomInt.nextInt(rightInterval-leftInterval+1)+leftInterval, innerCreateTree(levels-1), innerCreateTree(levels-1))
    }
    innerCreateTree(amountOfLevels)
  }
}
