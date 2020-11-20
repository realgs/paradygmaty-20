sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]//definicja drzew z wykladu

def breadthBT[A](tree: BT[A]): List[A] = {
  def bthelp[A](nodes: List[BT[A]]):List[A] = {
    nodes match {
      case Nil => Nil
      case Empty :: tail => bthelp(tail)
      case Node(value, left, right) :: tail => value :: bthelp(tail ++ List(left, right))
    }
  }
  bthelp(List(tree))
}//przejscie po elementach drzewa

//zad 1 3 pkt
def generateTree(height: Int, min: Int, max: Int):BT[Int]= {
  val r = scala.util.Random
  if(height<=0)
    return Empty
  def genHelp(currentHeight: Int):BT[Int] = {
    if(currentHeight+1==height)
      return Node(r.nextInt(max-min)+min,Empty,Empty)
    else
      return Node(r.nextInt(max-min)+min,genHelp(currentHeight+1),genHelp(currentHeight+1))
  }
  genHelp(0)
}
val t = generateTree(3,0,5)
val t2 = generateTree(6,100,200)
val t3 = generateTree(0,1,1)
breadthBT(t)
breadthBT(t2)
breadthBT(t3)

//zad2 3 pkt

def subTree(tree: BT[Int], tree2: BT[Int]):BT[Int] = {
  (tree,tree2) match{
    case (Empty,_) => Empty
    case (_,Empty) => Empty
    case (Node(elem,left,right),Node(elem2,left2,right2)) => Node(elem-elem2,subTree(left,left2),subTree(right,right2))
  }
}
val t4 = generateTree(3,0,5)
val t5 = generateTree(3,0,5)
val t6 = subTree(t4,t5)
breadthBT(t4)
breadthBT(t5)
breadthBT(t6)
