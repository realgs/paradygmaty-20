//zad 1 (3pkt)

sealed trait BT[+A] //
case object Empty extends BT[Nothing] //konstruktor wartosci
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A] //konstruktor wartosci

def createTree(depth:Int,maxValue:Int):BT[Int] = {
  if (maxValue < 0) throw new Exception("invalid maxValue for tree nodes")
  else if (depth < 0) throw new Exception("invalid depth value")
  //if (depth == 0) Empty

  val generator = scala.util.Random
  def createTreeHelper(depth:Int):BT[Int] =
    if (depth == 0) Empty
    else Node(generator.nextInt(maxValue+1),createTreeHelper(depth-1),createTreeHelper(depth-1))
  createTreeHelper(depth)
}
//Metoda do przeszukiwania drzewa wszerz
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

//zad2 (3pkt)

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

//zad3 (1pkt)

def removeDupl(tree1:BT[Int],tree2:BT[Int]):(BT[Int],BT[Int]) = {
  def removeDuplHelp(tree1:BT[Int],tree2:BT[Int]):(BT[Int],BT[Int]) =
    (tree1,tree2) match {
      case (Empty,Empty) => (Empty,Empty)
      case (Node(val1,lSubtree1,rSubtree1),Node(val2,lSubtree2,rSubtree2)) =>
        val left = removeDuplHelp(lSubtree1,lSubtree2)
        val right = removeDuplHelp(rSubtree1,rSubtree2)

        if (left._1 == left._2 && right._1 == right._2 && val1 == val2) (Empty,Empty)
        else if ( (left._1 != left._2 || right._1 != right._2) && val1 == val2 ) (Node(-1,left._1,right._1),Node(-1,left._2,right._2))
        else (Node(val1,left._1,right._1),Node(val2,left._2,right._2))
    }
    removeDuplHelp(tree1,tree2)
}

val testtree1 = Node(1,
                      Node(2,
                              Node(4,
                                    Node(9,Empty,Empty),
                                    Node(10,Empty,Empty)
                                    ),
                              Node(7,
                                    Node(11,Empty,Empty),
                                    Node(12,Empty,Empty)
                              )
                            ),
                      Node(3,
                              Node(5,
                                    Node(13,Empty,Empty),
                                    Node(14,Empty,Empty),
                                    ),
                              Node(8,
                                    Node(15,Empty,Empty),
                                    Node(16,Empty,Empty),
                              )
                            ),
                      )

val testtree2 = Node(1,
  Node(2,
    Node(4,
      Node(9,Empty,Empty),
      Node(10,Empty,Empty)
    ),
    Node(7,
      Node(11,Empty,Empty),
      Node(12,Empty,Empty)
    )
  ),
  Node(3,
    Node(5,
      Node(13,Empty,Empty),
      Node(14,Empty,Empty),
    ),
    Node(8,
      Node(15,Empty,Empty),
      Node(16,Empty,Empty),
    )
  ),
)

val output1 = removeDupl(testtree1,testtree2)
breadthBT(output1._1)
breadthBT(output1._2)

val testtree3 = Node(1,
  Node(2,
    Node(4,
      Node(9,Empty,Empty),
      Node(20,Empty,Empty) //10->20
    ),
    Node(7,
      Node(11,Empty,Empty),
      Node(12,Empty,Empty)
    )
  ),
  Node(3,
    Node(5,
      Node(13,Empty,Empty),
      Node(14,Empty,Empty),
    ),
    Node(8,
      Node(15,Empty,Empty),
      Node(16,Empty,Empty),
    )
  ),
)

val output2 = removeDupl(testtree1,testtree3)
breadthBT(output2._1)
breadthBT(output2._2)

val testtree4 = Node(1,
  Node(2,
    Node(4,
      Node(9,Empty,Empty),
      Node(20,Empty,Empty) //10->20
    ),
    Node(7,
      Node(11,Empty,Empty),
      Node(12,Empty,Empty)
    )
  ),
  Node(3,
    Node(5,
      Node(13,Empty,Empty),
      Node(21,Empty,Empty), //14->21
    ),
    Node(22, //8->22
      Node(15,Empty,Empty),
      Node(16,Empty,Empty),
    )
  ),
)

val output3 = removeDupl(testtree1,testtree4)
breadthBT(output3._1)
breadthBT(output3._2)

