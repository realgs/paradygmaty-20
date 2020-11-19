import scala.util.Random
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](value:A, left:BT[A], right:BT[A]) extends BT[A]

object L4 {
  // Zad 1 3pkt.
  def generateTree(boundaries:(Int,Int),heightOfTree:Int):BT[Int]={
    def helper(leftLevels:Int):BT[Int]={
      if(leftLevels == 1) Node(Random.nextInt(boundaries._2 - boundaries._1 + 1) + boundaries._1,Empty,Empty)
      else Node(Random.nextInt(boundaries._2 - boundaries._1 + 1) + boundaries._1,helper(leftLevels - 1),helper(leftLevels - 1))
    }
    if(heightOfTree <= 0) Empty
    else helper(heightOfTree)
  }

  def breadthSearch[A](tree:BT[A]):List[A]={
    def helper(queue:List[BT[A]]):List[A]={
      queue match {
        case Nil => Nil
        case Empty::tail => helper(tail)
        case Node(value,left,right)::tail =>  value::helper(tail:::List(left,right))
      }
    }
    helper(List(tree))
  }
  
}

