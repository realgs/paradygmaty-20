sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

class L4 {

  //zadanie 1 (3 pkt)
  def generateTree(n:Int, down:Int, up:Int):BT[Int] ={
    val r = scala.util.Random
    def helper(n: Int): BT[Int] =
      if (n == 0) Node(down + r.nextInt(up - down), Empty, Empty)
      else Node(down + r.nextInt(up - down), helper(n - 1), helper(n - 1))

    if(down<0||up<0||down>up||n<=0) Empty
    else helper(n-1)
  }

  //zadanie 2 (3 pkt)
  def subOfTrees(tree1:BT[Int],tree2:BT[Int]):BT[Int] =
    (tree1,tree2) match{
      case (Node(v1,l1,r1),Node(v2,l2,r2)) => Node(v1-v2,subOfTrees(l1,l2),subOfTrees(r1,r2))
      case (Empty,Empty) => Empty
      case(_,_) => throw new Exception("Incorrect Tree")
    }

  //zadanie 3 (4 pkt)
  def sameSubTrees[A](tree1:BT[A],tree2:BT[A]):(BT[A],BT[A]) ={

  }

}
