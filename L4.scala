sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

class L4 {
  //zadanie 1 (3 pkt)
  def generateTree(n:Int, down:Int, up:Int):BT[Int] ={
    val r = scala.util.Random
    def helper(n: Int): BT[Int] =
      if (n == 0) Node(down + r.nextInt(up), Empty, Empty)
      else Node(down + r.nextInt(up), helper(n - 1), helper(n - 1))

    if(down<0||up<0||down>up||n<=0) Empty
    else helper(n - 1)
  }
}
