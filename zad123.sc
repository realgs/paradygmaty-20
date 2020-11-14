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
val treeONE = createTree(3,5)
val treeTWO = createTree(3,5)
breadthBT(treeONE)
breadthBT(treeTWO)

//zad2 3pkt

def createTree2(tree1:BT[Int],tree2:BT[Int]):BT[Int] = {
  def createTree2Helper(tree1:BT[Int],tree2:BT[Int]):BT[Int] =
    (tree1,tree2) match {
      case (Empty,Empty) => Empty
      case (Node(val1,lSubtree1,rSubtree1),Node(val2,lSubtree2,rSubtree2)) => Node(val1-val2,createTree2Helper(lSubtree1,lSubtree2),createTree2Helper(rSubtree1,rSubtree2))
    }
  createTree2Helper(tree1,tree2)
}

val treeOUTPUT = createTree2(treeONE,treeTWO)
breadthBT(treeOUTPUT)

//zad3 1pkt

