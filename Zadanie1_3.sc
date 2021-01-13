import scala.util.Random
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](value:A, left:BT[A], right:BT[A]) extends BT[A]

//Zadanie 1 (3pkt)
def generateTree(range:(Int,Int), height:Int):BT[Int]={
  def helper(leftLevels:Int):BT[Int]={
    if(leftLevels == 1) Node(Random.nextInt(range._2 - range._1 + 1) + range._1,Empty,Empty)
    else Node(Random.nextInt(range._2 - range._1 + 1) + range._1,helper(leftLevels - 1),helper(leftLevels - 1))
  }
  if(height <= 0) Empty
  else helper(height)
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

val tree1= generateTree((1,10),3)
val tree2 = generateTree((1,10),0)

breadthSearch(tree1)
breadthSearch(tree2)

//Zadanie 2 (3pkt)
def differenceElement(firstTree:BT[Int],secondTree:BT[Int]):BT[Int]={
  def helper(tree1:BT[Int], tree2:BT[Int]):BT[Int]={
    (tree1,tree2) match{
      case (Empty,Empty) => Empty
      case (Empty,_) | (_,Empty) => throw new Exception("Drzewa nie są sobie równe")
      case (Node(value1,left1,right1),Node(value2,left2,right2)) => Node(value1-value2,helper(left1,left2),helper(right1,right2))
    }
  }
  helper(firstTree,secondTree)
}

val tree3 = generateTree((2,8),3)
breadthSearch(differenceElement(tree1,tree3))



//Zadanie 3
//przechodząc po drzewie wgłąb (1pkt)
def removeDuplicates(firstTree:BT[Int],secondTree:BT[Int]):(BT[Int],BT[Int])={
  def helper(tree1:BT[Int], tree2:BT[Int]):BT[Int]={
    (tree1,tree2) match{
      case (Empty,Empty) => Empty
      case (Empty,_) | (_,Empty) => throw new Exception("Drzewa nie są tej samej wielkosci")
      case (Node(value1,left1,right1),Node(value2,left2,right2)) => {
        val leftSubTree = helper(left1,left2)
        val rightSubTree = helper(right1,right2)

        if(leftSubTree == Empty && rightSubTree == Empty){if(value1==value2)Empty else Node(value1,Empty,Empty)}
        else if(value1 == value2) Node(-1,leftSubTree,rightSubTree)
        else Node(value1,leftSubTree,rightSubTree)
      }
    }
  }
  (helper(firstTree,secondTree),helper(secondTree,firstTree))
}

removeDuplicates(tree1, tree3)
removeDuplicates(Empty, Empty)