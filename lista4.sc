// Konrad Karanowski
import scala.util.Random


// Binary Tree
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


// Binary Tree helpers
def breadth[A](tree: BT[A]): List[A] =
{
  def breadthBTRec(queue: List[BT[A]]): List[A] =
    queue match
    {
      case Empty :: tail => breadthBTRec(tail)
      case Node(a, leftChild, rightChild) :: tail => a :: breadthBTRec(tail ::: List(leftChild, rightChild))
      case _ => Nil
    }
  breadthBTRec(List(tree))
}


def generateTreeWithRule(rule: Int => Int)(n: Int): BT[Int] =
{
  if (n == 0) Empty
  else Node(rule(n), generateTreeWithRule(rule)(n - 1), generateTreeWithRule(rule)(n - 1))
}


// testy
breadth(generateTreeWithRule((a: Int) => a + 1)(3)) == List(4, 3, 3, 2, 2, 2, 2)
breadth(generateTreeWithRule((a: Int) => a * 2)(2)) == List(4, 2, 2)
breadth(generateTreeWithRule((a: Int) => -1)(3)) == List(-1, -1, -1, -1, -1, -1, -1)


def treeHeight[A](tree: BT[A]): Int =
{
  tree match
  {
    case Node(_, leftChild, rightChild) => 1 + Math.max(treeHeight(leftChild), treeHeight(rightChild))
    case _ => 0
  }
}


// testy
treeHeight(Node(1, Empty, Empty)) == 1
treeHeight(Empty) == 0
treeHeight(Node(1, Empty, Node(2, Empty, Empty))) == 2


def isFull[A](tree: BT[A]): Boolean =
{
  tree match
  {
    case Node(_, Empty, Empty) => true
    case Node(_, leftChild, rightChild) => isFull(leftChild) && isFull(rightChild)
    case _ => false
  }
}


// testy
isFull(Node(1, Empty, Empty))
!isFull(Empty)
!isFull(Node(1, Node(0, Empty, Empty), Empty))
isFull(Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))


// Zadanie 1 (3 pkt)
def getRandomInt(a: Int, b: Int): Int =
{
  if ((a <= 0) || (b <= 0)) throw new Exception("a and b has to be poisitive")
  else if (b <= a) throw new Exception("b has to be greater than a")
  else Random.between(a, b)
}


def generateTree(n: Int, a: Int, b: Int): BT[Int] =
{
  if (n < 0) throw new Exception("Tree height cannot be negative")
  def generateTree(depth: Int): BT[Int] =
  {
    if (depth == 0) Empty
    else Node(getRandomInt(a, b), generateTree(depth - 1), generateTree(depth - 1))
  }
  generateTree(n)
}


// testy
isFull(generateTree(5, 1, 3))
breadth(generateTree(3, 2, 3)) == List(2, 2, 2, 2, 2, 2, 2)
isFull(generateTree(8, 1, 128))
treeHeight(generateTree(10, 1, 2)) == 10
try {generateTree(-2, 3, 12)} catch {case e : Throwable => e.printStackTrace()}
try {generateTree(10, 0, 11)} catch {case e : Throwable => e.printStackTrace()}
try {generateTree(10, 2, 0)} catch {case e : Throwable => e.printStackTrace()}
try {generateTree(10, 2, 1)} catch {case e : Throwable => e.printStackTrace()}


// Zadanie 2 (3 pkt)
def generateDifferenceTree(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
{
  if (!isFull(tree1) || !isFull(tree2)) throw new Exception("Trees has to be full")
  if (treeHeight(tree1) != treeHeight(tree2)) throw new Exception("Trees has to be same size")
  def generateDifferenceTreeRec(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    (tree1, tree2) match
    {
      case (Node(a, lc1, rc1), Node(b, lc2, rc2)) => Node(a - b, generateDifferenceTreeRec(lc1, lc2), generateDifferenceTreeRec(rc1, rc2))
      case _ => Empty
    }
  generateDifferenceTreeRec(tree1, tree2)
}


// testy
isFull(generateDifferenceTree(generateTree(3, 2, 5), generateTree(3, 2, 5)))
breadth(generateDifferenceTree(generateTree(3, 2, 3), generateTree(3, 2, 3))) == List(0, 0, 0, 0, 0, 0, 0)
breadth(generateDifferenceTree(
  Node(12, Node(5, Empty, Empty), Node(17, Empty, Empty)),
  Node(10, Node(3, Empty, Empty), Node(-12, Empty, Empty))
)) == List(2, 2, 29)


// Zadanie 3 a (1 pkt)
def dropDuplicatesDFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
{
  if (!isFull(tree1) || !isFull(tree2)) throw new Exception("Trees has to be full")
  if (treeHeight(tree1) != treeHeight(tree2)) throw new Exception("Trees has to be same size")
  def dropDuplicatesDFSRec(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
  {
    (tree1, tree2) match
    {
      case (Node(a, leftChild1, rightChild1), Node(b, leftChild2, rightChild2)) =>
      {
        val (leftS1, leftS2) = dropDuplicatesDFSRec(leftChild1, leftChild2)
        val (rightS1, rightS2) = dropDuplicatesDFSRec(rightChild1, rightChild2)
        if (a == b)
        {
          (leftS1, rightS1) match
          {
            case (Empty, Empty) => (Empty, Empty)
            case _ => (Node(-1, leftS1, rightS1), Node(-1, leftS2, rightS2))
          }
        }
        else (Node(a, leftS1, rightS1), Node(b, leftS2, rightS2))
      }
      case _ => (Empty, Empty)
    }
  }
  dropDuplicatesDFSRec(tree1, tree2)
}


// testy
dropDuplicatesDFS(generateTree(3, 1, 2), generateTree(3, 1, 2)) == (Empty, Empty)
val (tree1a, tree2a) = dropDuplicatesDFS(generateTreeWithRule((a: Int) => 0)(3), generateTreeWithRule((a: Int) => a - 1)(3))
breadth(tree1a) == List(0, 0, 0)
breadth(tree2a) == List(2, 1, 1)
val (tree1b, tree2b) = dropDuplicatesDFS(generateTreeWithRule((a: Int) => a % 2)(3), generateTreeWithRule((a: Int) => 0)(3))
breadth(tree1b) == List(1, -1, -1, 1, 1, 1, 1)
breadth(tree2b) == List(0, -1, -1, 0, 0, 0, 0)
try {dropDuplicatesDFS(Empty, Empty) == (Empty, Empty)} catch {case e : Throwable => e.printStackTrace()}


// Zadanie 3 b (3 pkt)
def breadthSame(tree1: BT[Int], tree2: BT[Int]): Boolean =
{
  def breadthSameRec(queue: List[(BT[Int], BT[Int])]): Boolean =
  {
    queue match
    {
      case (Empty, Empty) :: tail => breadthSameRec(tail)
      case (Node(a, leftChild1, rightChild1), Node(b, leftChild2, rightChild2)) :: tail =>
        if (a == b) breadthSameRec(tail ::: List((leftChild1, leftChild2), (rightChild1, rightChild2)))
        else false
      case _ => true
    }
  }
  breadthSameRec(List((tree1, tree2)))
}


def dropDuplicatesBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
{
  if (!isFull(tree1) || !isFull(tree2)) throw new Exception("Trees has to be full")
  if (treeHeight(tree1) != treeHeight(tree2)) throw new Exception("Trees has to be same size")
  def dropDuplicatesBFSRec(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
  {
    (tree1, tree2) match
    {
      case (Node(a, leftChild1, rightChild1), Node(b, leftChild2, rightChild2)) =>
        (breadthSame(leftChild1, leftChild2), breadthSame(rightChild1, rightChild2)) match
        {
          case (true, true) =>
            if (a == b) (Empty, Empty)
            else (Node(a, Empty, Empty), Node(b, Empty, Empty))
          case (true, false) =>
            {
              val (leftS1, leftS2) = dropDuplicatesBFSRec(leftChild1, leftChild2)
              if (a == b) (Node(-1, leftS1, Empty), Node(-1, leftS2, Empty))
              else (Node(a, leftS1, Empty), Node(b, leftS2, Empty))
            }
          case (false, true) =>
            {
              val (rightS1, rightS2) = dropDuplicatesBFSRec(rightChild1, rightChild2)
              if (a == b) (Node(-1, Empty, rightS1), Node(-1, Empty, rightS2))
              else (Node(a, Empty, rightS1), Node(b, Empty, rightS2))
            }
          case _ =>
            {
              val (leftS1, leftS2) = dropDuplicatesBFSRec(leftChild1, leftChild2)
              val (rightS1, rightS2) = dropDuplicatesBFSRec(rightChild1, rightChild2)
              if (a == b) (Node(-1, leftS1, rightS1), Node(-1, leftS2, rightS2))
              else (Node(a, leftS1, rightS1), Node(b, leftS2, rightS2))
            }
        }
      case _ => (Empty, Empty)
    }
  }
  dropDuplicatesBFSRec(tree1, tree2)
}



//test
dropDuplicatesBFS(generateTree(3, 1, 2), generateTree(3, 1, 2)) == (Empty, Empty)
val (tree1a, tree2a) = dropDuplicatesBFS(generateTreeWithRule((a: Int) => 0)(3), generateTreeWithRule((a: Int) => a - 1)(3))
breadth(tree1a) == List(0, 0, 0)
breadth(tree2a) == List(2, 1, 1)
val (tree1b, tree2b) = dropDuplicatesBFS(generateTreeWithRule((a: Int) => a % 2)(3), generateTreeWithRule((a: Int) => 0)(3))
breadth(tree1b) == List(1, -1, -1, 1, 1, 1, 1)
breadth(tree2b) == List(0, -1, -1, 0, 0, 0, 0)
try {dropDuplicatesBFS(Empty, Empty) == (Empty, Empty)} catch {case e : Throwable => e.printStackTrace()}


// Zadanie 4 (5 pkt)
def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
{
  if (n <= 0) throw new Exception("n has to be positive")
  else if (m < 0) throw new Exception("m cannot be negative")
  def eachNElementRec(list: LazyList[A], idx: Int): LazyList[A] =
  {
    if (idx >= m) LazyList()
    else
    {
      list match
      {
        case head #:: tail =>
          if (idx % n == 0) head #:: eachNElementRec(tail, idx + 1)
          else eachNElementRec(tail, idx + 1)
        case _ => LazyList()
      }
    }
  }
  eachNElementRec(list, 0)
}


// Testy
eachNElement(LazyList(0, 1, 2, 3, 4, 5, 6), 3, 6).take(8).toList == List(0, 3)
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).take(6).toList == List(5, 3)
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).take(6).toList == List(5, 3)
eachNElement(LazyList(), 100, 1000000000) == LazyList()
eachNElement(LazyList(1, 2, 3, 4, 5, 6), 1, 0) == LazyList()
eachNElement(LazyList("A", "X", "Z", "L", "O", "J", "A", "Q"), 3, 100).take(8).toList == List("A", "L", "A")
try {eachNElement(LazyList(10), 0, 1)} catch {case e : Throwable => e.printStackTrace()}
try {eachNElement(LazyList(1, 2), 2, -1)} catch {case e : Throwable => e.printStackTrace()}


// Zadanie 5 (5 pkt)
def lazyOperation[A](f: A => A => A)(list1: LazyList[A], list2: LazyList[A]): LazyList[A] =
{
  (list1, list2) match
  {
    case (LazyList(), list2) => list2
    case (list1, LazyList()) => list1
    case (head1 #:: tail1, head2 #:: tail2) => f(head1)(head2) #:: lazyOperation(f)(tail1, tail2)
    case _ => LazyList()
  }
}


val + = (a: Int) => (b: Int) => a + b
val - = (a: Int) => (b: Int) => a - b
val * = (a: Int) => (b: Int) => a * b
val / = (a: Int) => (b: Int) => a / b


lazyOperation(+)(LazyList(1, 2, 3, 4, 5), LazyList(-1, -2, -3, -4)).take(8).toList == List(0, 0, 0, 0, 5)
lazyOperation(-)(LazyList(-1, 0, -2, -5), LazyList(-2, -2, -2, -2)).take(8).toList == List(1, 2, 0, -3)
lazyOperation(*)(LazyList(1, 2, 3, 4), LazyList()).take(8).toList == List(1, 2, 3, 4)
lazyOperation(/)(LazyList(-1, -2, -3), LazyList(1, 2, 3)).toList == List(-1, -1, -1)
lazyOperation(-)(LazyList(), LazyList()).take(8).toList == Nil
lazyOperation(-)(LazyList(), LazyList(1, 1, 1, 1, 1)).take(8).toList == List(1, 1, 1, 1, 1)