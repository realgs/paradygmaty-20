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

  // Zad 2 3pkt.
  def generateTreeOfDifference(firstTree:BT[Int],secondTree:BT[Int]):BT[Int]={
    def helper(fTree:BT[Int],sTree:BT[Int]):BT[Int]={
      (fTree,sTree) match{
        case (Empty,Empty) => Empty
        case (Empty,_) | (_,Empty) => throw new Exception("Drzewa nie są sobie równe")
        case (Node(v1,l1,r1),Node(v2,l2,r2)) => Node(v1-v2,helper(l1,l2),helper(r1,r2))
      }
    }
    helper(firstTree,secondTree)
  }
}

