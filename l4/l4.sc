import scala.util.Random
sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//Zadanie1 (3pkt)
def generateTree(n: Int)(minValue: Int)(maxValue: Int):BT[Int] = {
  val r = new Random()
  def genSubTree(n: Int): BT[Int] =
    n match {
      case 0 => Empty
      case _ => Node(minValue + r.nextInt(maxValue-minValue+1), genSubTree(n-1), genSubTree(n-1))
    }
  genSubTree(n)
}

def treeDepth[A](tree: BT[A]): Int ={
  def checkTreeDepthIter(subtree: BT[A])(k: Int): Int  =
    subtree match{
      case Empty => k
      case Node(_ , left, right) => checkTreeDepthIter(left)(k+1)&checkTreeDepthIter(right)(k+1)
    }
  checkTreeDepthIter(tree)(0)
}

def isTreeDepthN[A](tree: BT[A])(n: Int): Boolean ={
  treeDepth(tree) == n
}
//isTreeDepthNTests
isTreeDepthN(Node(1,Empty,Empty))(1)
isTreeDepthN(Node(5,Node(10,Node(4,Empty,Empty),
  Node(6,Empty,Empty)),Node(3,Node(4,Empty,Empty),
  Node(7,Empty,Empty))))(3)

def isTreeFull[A](tree: BT[A]): Boolean ={
  def checkSubTree(subtree: BT[A]): Boolean  =
    subtree match {
      case Empty => true
      case Node(_ , Empty, Empty) => true
      case Node(_ , Empty, right) => false
      case Node(_ , left, Empty) => false
      case Node(_ , left, right) => (treeDepth(left) == treeDepth(right))&checkSubTree(left)&checkSubTree(right)
    }
  checkSubTree(tree)
}
//isTreeFullTests
!isTreeFull(Node(1, Node(2, Empty, Empty), Empty))
!isTreeFull(Node(1,Empty, Node(2, Empty, Empty)))
isTreeFull(Node(1,Empty,Empty))
!isTreeFull(Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Empty, Empty)))


//generateTreeTests
isTreeFull(generateTree(1)(1)(3))
isTreeDepthN(generateTree(1)(1)(3))(1)
isTreeFull(generateTree(3)(1)(10))
isTreeDepthN(generateTree(3)(1)(10))(3)

//Zadanie2 (3pkt)
def subtractTrees(tree_1: BT[Int])(tree_2: BT[Int]): BT[Int] ={
  def subtractSubTree(subtree_1: BT[Int])(subtree_2: BT[Int]): BT[Int] =
    (subtree_1, subtree_2) match{
      case(Empty, Empty) => Empty
      case (Empty, Node(_, _, _)) => throw new Exception("Drzewa różnej głębokości")
      case (Node(_, _, _), Empty) => throw new Exception("Drzewa różnej głębokości")
      case(Node(elem_1, left_1, right_1), Node(elem_2, left_2, right_2))
      => Node(elem_1-elem_2, subtractSubTree(left_1)(left_2), subtractSubTree(right_1)(right_2))
    }
  subtractSubTree(tree_1)(tree_2)
}
val tree_0 = generateTree(2)(0)(0)
val tree_1 = generateTree(2)(4)(6)
val tree_2 = generateTree(2)(1)(3)
val tree_1_preGen = Node(3,Node(8,Empty,Empty),Node(6,Empty,Empty))
val tree_2_preGen = Node(2,Node(3,Empty,Empty),Node(1,Empty,Empty))
subtractTrees(tree_1)(tree_2) != tree_0
subtractTrees(tree_1_preGen)(tree_2_preGen) ==
  Node(1,Node(5,Empty,Empty),Node(5,Empty,Empty))
subtractTrees(generateTree(4)(8)(8))(generateTree(4)(4)(4)) ==
  Node(4,Node(4,Node(4,Node(4,Empty,Empty),Node(4,Empty,Empty)),Node(4,Node(4,Empty,Empty),
    Node(4,Empty,Empty))),Node(4,Node(4,Node(4,Empty,Empty),Node(4,Empty,Empty)),
    Node(4,Node(4,Empty,Empty),Node(4,Empty,Empty))))




sealed trait nlist[A]
case class Koniec[A]() extends nlist[A]
case class Element[A](elem: A, list: nlist[A])extends nlist[A]

sealed trait llist[A]
case class LKoniec[A]() extends llist[A]
case class LElement[A](elem: A, tail: ()=>llist[A])extends llist[A]

def toNList[A](lxs: llist[A]): nlist[A] =
  lxs match{
    case LKoniec() => Koniec()
    case LElement(hd, tail) => Element(hd, toNList(tail()))
  }

//Zadanie4 (5pkt)
def eachNElement[A](lxs: llist[A], n :Int, endIndex: Int): llist[A] = {
  def eachNElementIter(lys: llist[A], k :Int): llist[A] = {
    if(k<endIndex-1) {
      (k % n, lys) match {
        case (_, LKoniec()) => LKoniec()
        case (0, LElement(hd, tail)) => LElement(hd,()=>eachNElementIter(tail(), k + 1))
        case (_, LElement(_, tail)) => eachNElementIter(tail(), k + 1)
      }
    }
    else (k % n, lys) match {
      case (_, LKoniec()) => LKoniec()
      case (0, LElement(hd, _ )) => LElement(hd,()=>LKoniec())
      case ( _, _ ) => LKoniec()
    }
  }
  eachNElementIter(lxs, 0)
}

toNList(eachNElement(LElement(5, ()=>LElement(6, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))), 2, 3)) ==
  Element(5,Element(3,Koniec()))
toNList(eachNElement(LElement(5, ()=>LElement(6, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))), 2, 4)) ==
  Element(5,Element(3,Koniec()))
toNList(eachNElement(LElement(5, ()=>LElement(6, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))), 2, 5)) ==
  Element(5,Element(3,Element(1,Koniec())))


