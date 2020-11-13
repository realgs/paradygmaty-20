//zad 1 3pkt

sealed trait BT[+A] //
case object Empty extends BT[Nothing] //konstruktor wartosci
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A] //konstruktor wartosci

def createTree(depth:Int,maxValue:Int):BT[Int] = {
  if (maxValue < 0) throw new Exception("invalid maxValue for tree nodes")
  else if (depth < 0) throw new Exception("invalid depth value")
  if (depth == 0) Empty

  val generator = scala.util.Random
  def createTreeHelper(depth:Int):BT[Int] =
    if (depth == 0) Empty
    else Node(generator.nextInt(maxValue+1),createTreeHelper(depth-1),createTreeHelper(depth-1))
  createTreeHelper(depth)
}

def breadthBT[A](tree: BT[A]): List[A] = {
  def breadthBThelp[A](nodeQueue: List[BT[A]]): List[A] =
    nodeQueue match {
      case Nil => Nil
      case Empty :: tail => breadthBThelp(tail)
      case Node(value, leftSubtree, rightSubtree) :: tail => value :: breadthBThelp(tail ::: List(leftSubtree, rightSubtree))
    }
  breadthBThelp (List(tree))
}

breadthBT(createTree(3,100))
//breadthBT(createTree(3,-1))
//breadthBT(createTree(-1,100))
breadthBT(createTree(3,0))