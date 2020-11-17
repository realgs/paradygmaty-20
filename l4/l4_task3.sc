import scala.util.Random
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


//metody pomocnicze
def generateTree(n: Int)(minValue: Int)(maxValue: Int):BT[Int] = {
  val r = new Random()
  def genSubTree(n: Int): BT[Int] =
    n match {
      case 0 => Empty
      case _ => Node(minValue + r.nextInt(maxValue-minValue+1), genSubTree(n-1), genSubTree(n-1))
    }
  genSubTree(n)
}

def isTreeFull[A](tree: BT[A]): Boolean ={
  def checkSubTree(subtree: BT[A]): Boolean  =
    subtree match {
      case Empty => true
      case Node(_ , Empty, right) => if(right==Empty) true else false
      case Node(_ , left, Empty) => if(left==Empty) true else false
      case Node(_ , left, right) => checkSubTree(left)&checkSubTree(right)
    }
  checkSubTree(tree)
}

def treeDepth[A](tree: BT[A]): Int ={
  def checkTreeDepthIter(subtree: BT[A])(k: Int): Int  =
    subtree match{
      case Empty => k
      case Node(_ , left, right) => checkTreeDepthIter(left)(k+1)&checkTreeDepthIter(right)(k+1)
    }
  checkTreeDepthIter(tree)(0)
}

treeDepth(generateTree(3)(1)(10)) == 3
treeDepth(Node(5,Node(10,Node(4,Empty,Empty),
  Node(6,Empty,Empty)),Node(3,Node(4,Empty,Empty),
  Node(7,Empty,Empty)))) == 3

val tree_1 = generateTree(2)(4)(6)
val tree_2 = generateTree(2)(1)(3)

//Zadanie3 (4pkt)
def areTreesEqualDepth(tree_1: BT[Int])(tree_2: BT[Int]): Boolean =
  (tree_1, tree_2) match{
    case (Empty, Empty) => true
    case (Node(elem_1, left_1, right_1), Node(elem_2, left_2, right_2)) =>
      (elem_1==elem_2)&areTreesEqualDepth(left_1)(left_2)&areTreesEqualDepth(right_1)(right_2)
  }

areTreesEqualDepth(tree_2)(tree_2)
!areTreesEqualDepth(tree_1)(tree_2)
areTreesEqualDepth(Empty)(Empty)
areTreesEqualDepth(tree_1)(tree_1)

def areTreesEqualBreadth(tree_1: BT[Int])(tree_2: BT[Int]): Boolean ={
  def BreadthIter(subTree_1: List[BT[Int]])(subTree_2: List[BT[Int]]): Boolean =
    if(subTree_1.isEmpty&subTree_2.isEmpty) true
    else (subTree_1.head,subTree_2.head ) match{
      case (Empty,Empty) => BreadthIter(subTree_1.tail)(subTree_2.tail)
      case (Node(elem_1, left_1, right_1), Node(elem_2, left_2, right_2)) =>
        (elem_1==elem_2)&BreadthIter(subTree_1.tail:::List(left_1, right_1))(subTree_2.tail :::List(left_2, right_2))
    }
  BreadthIter(List(tree_1))(List(tree_2))
}
areTreesEqualBreadth(tree_2)(tree_2)
!areTreesEqualBreadth(tree_1)(tree_2)
areTreesEqualBreadth(Empty)(Empty)
areTreesEqualBreadth(tree_1)(tree_1)



def deleteDuplicates(check: BT[Int]=>BT[Int]=>Boolean)(tree_1: BT[Int])(tree_2: BT[Int]): (BT[Int], BT[Int]) ={
  def deleteDuplicatesIter(tree_1: BT[Int])(tree_2: BT[Int]): (BT[Int], BT[Int]) =
    (tree_1, tree_2) match{
      case (Node(elem_1, left_1, right_1), Node(elem_2, left_2, right_2 )) =>
        (elem_1==elem_2, check(left_1)(left_2),check(right_1)(right_2)) match{
          case (true, true, true) => (Empty, Empty)
          case (false, false, false) => (tree_1, tree_2)
          case (true, true, false) =>
            val (subtree_1, subtree_2) = deleteDuplicatesIter(right_1)(right_2)
            (Node(-1, Empty, subtree_1), Node(-1, Empty, subtree_2))
          case (false, true, false) =>
            val (subtree_1, subtree_2) = deleteDuplicatesIter(right_1)(right_2)
            (Node(elem_1, Empty, subtree_1), Node(elem_2, Empty, subtree_2))
          case (true, false, true) =>
            val (subtree_1, subtree_2) = deleteDuplicatesIter(left_1)(left_2)
            (Node(-1, subtree_1, Empty), Node(-1, subtree_2, Empty))
          case (false, false, true) =>
            val (subtree_1, subtree_2) = deleteDuplicatesIter(left_1)(left_2)
            (Node(elem_1, subtree_1, Empty), Node(elem_2, subtree_2, Empty))
          case (false, true, true) => (Node(elem_1, Empty, Empty), Node(elem_2, Empty, Empty))
          case (true, false, false) => (Node(-1, left_1, right_1), Node(-1, left_2, right_2))
        }
    }
  if(treeDepth(tree_1)==treeDepth(tree_2)&isTreeFull(tree_1)&isTreeFull(tree_1)){
    deleteDuplicatesIter(tree_1)(tree_2)
  }else throw new Exception("Dane wejściowe nie spełaniają warunków zadania")
}
val tree_3 = generateTree(2)(3)(6)
val tree_4 = generateTree(2)(1)(3)
val tree_5 = Node(2,Node(6,Empty,Empty),Node(-1,Empty,Empty))
val tree_6 = Node(2,Node(7,Empty,Empty),Node(-1,Empty,Empty))
val tree_7 = Node(2,Node(8,Empty,Empty),Node(0,Empty,Empty))
//Depth
deleteDuplicates(areTreesEqualDepth)(tree_4)(tree_4) == (Empty,Empty)
deleteDuplicates(areTreesEqualDepth)(tree_3)(tree_3) == (Empty,Empty)
deleteDuplicates(areTreesEqualDepth)(tree_3)(tree_4) != (Empty,Empty)
deleteDuplicates(areTreesEqualDepth)(tree_5)(tree_6) ==
  (Node(-1,Node(6,Empty,Empty),Empty),Node(-1,Node(7,Empty,Empty),Empty))
deleteDuplicates(areTreesEqualDepth)(tree_6)(tree_7) ==
  (Node(-1,Node(7,Empty,Empty),Node(-1,Empty,Empty)),
    Node(-1,Node(8,Empty,Empty),Node(0,Empty,Empty)))
//Breadth
deleteDuplicates(areTreesEqualBreadth)(tree_4)(tree_4) == (Empty,Empty)
deleteDuplicates(areTreesEqualBreadth)(tree_3)(tree_3) == (Empty,Empty)
deleteDuplicates(areTreesEqualBreadth)(tree_3)(tree_4) != (Empty,Empty)
deleteDuplicates(areTreesEqualBreadth)(tree_5)(tree_6) ==
  (Node(-1,Node(6,Empty,Empty),Empty),Node(-1,Node(7,Empty,Empty),Empty))
deleteDuplicates(areTreesEqualBreadth)(tree_6)(tree_7) ==
  (Node(-1,Node(7,Empty,Empty),Node(-1,Empty,Empty)),
    Node(-1,Node(8,Empty,Empty),Node(0,Empty,Empty)))

