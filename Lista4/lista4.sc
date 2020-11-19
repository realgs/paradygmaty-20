import scala.annotation.tailrec

//Jakub Kochański
//Zad1 (3pkt)
sealed trait BinaryTree[+A]

case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

case object Empty extends BinaryTree[Nothing]

val generateTree: Int => (Int, Int) => BinaryTree[Int] = depth => (from, to) => {
  def generateValue: Int = from + scala.util.Random.nextInt(to - from + 1)

  def generateNode(value: Int, currentDepth: Int): BinaryTree[Int] = {
    if (currentDepth == depth) Empty
    else Node(value, generateNode(generateValue, currentDepth + 1), generateNode(generateValue, currentDepth + 1))
  }

  if (depth < 0 || from > to || from <= 0) throw new IllegalArgumentException("generateTree")
  generateNode(generateValue, 0)
}

val testTree: BinaryTree[Int] => (Int, Int) => Int => Boolean = treeToTest => (min, max) => depth => {
  def countNodes(binaryTree: BinaryTree[Int])(currentDepth: Int): Int = {
    binaryTree match {
      case Empty => 0
      case Node(value, left, right) =>
        assert(value >= min && value <= max && currentDepth <= depth, "Illegal node value!")
        1 + countNodes(left)(currentDepth + 1) + countNodes(right)(currentDepth + 1)
    }
  }

  countNodes(treeToTest)(1) == scala.math.pow(2, depth) - 1
}

testTree(generateTree(3)(1, 5))(1, 5)(3)
testTree(generateTree(8)(1, 100))(1, 100)(8)
testTree(generateTree(15)(100, 400))(100, 400)(15)

//Zad2 (3pkt)
val treesDifference: BinaryTree[Int] => BinaryTree[Int] => BinaryTree[Int] = minuedTree => subtrahentTree => (minuedTree, subtrahentTree) match {
  case (Empty, Empty) => Empty
  case (Empty, _) => throw new IllegalArgumentException("Trees are not of the same depth!")
  case (_, Empty) => throw new IllegalArgumentException("Trees are not of the same depth!")
  case (Node(value1, left1, right1), Node(value2, left2, right2)) =>
    Node(value1 - value2, treesDifference(left1)(left2), treesDifference(right1)(right2))
}

val emptySubtractedTree = treesDifference(Empty)(Empty)
val oneNodeSubtractedTree = treesDifference(generateTree(1)(1, 5))(generateTree(1)(2, 5))
val smallSubtractedTree = treesDifference(generateTree(5)(1, 10))(generateTree(5)(11, 20))
val bigSubtractedTree = treesDifference(generateTree(20)(1, 1000))(generateTree(20)(1, 500))
testTree(emptySubtractedTree)(0, 0)(0)
testTree(oneNodeSubtractedTree)(-4, 3)(1)
testTree(smallSubtractedTree)(-19, -1)(5)
testTree(bigSubtractedTree)(-499, 999)(20)

//Zad3 (4pkt)
//Depth search (1pkt)
def deleteDuplicatesD(t1: BinaryTree[Int], t2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) = {
  def createUniqueTree(tree: BinaryTree[Int], tree2: BinaryTree[Int]): BinaryTree[Int] = (tree, tree2) match {
    case (Empty, Empty) => Empty
    case (Empty, _) | (_, Empty) => throw new Exception("Trees are not symmetrical!")
    case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
      val leftSubtree = createUniqueTree(l1, l2)
      val rightSubtree = createUniqueTree(r1, r2)

      if (leftSubtree == Empty && rightSubtree == Empty) {
        if (v1 == v2) Empty
        else Node(v1, Empty, Empty)
      }
      else if (v1 == v2) Node(-1, leftSubtree, rightSubtree)
      else Node(v1, leftSubtree, rightSubtree)
  }

  (createUniqueTree(t1, t2), createUniqueTree(t2, t1))
}

val t1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
val t2 = Node(1, Node(7, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(8, Empty, Empty)))
val expectedResult = (Node(-1, Node(2, Empty, Empty), Node(-1, Empty, Node(7, Empty, Empty))), Node(-1, Node(7, Empty, Empty), Node(-1, Empty, Node(8, Empty, Empty))))
val t3 = generateTree(10)(1, 100)

deleteDuplicatesD(t1, t2) == expectedResult
deleteDuplicatesD(t3, t3) == (Empty, Empty)
deleteDuplicatesD(Empty, Empty) == (Empty, Empty)

//Breadth search (3pkt)
def treeToVector(t: BinaryTree[Int]): Vector[Int] = {
  @tailrec
  def traverseTree(queue: List[BinaryTree[Int]], result: Vector[Int]): Vector[Int] = queue match {
    case Nil => result.reverse
    case Empty :: t => traverseTree(t, result)
    case Node(v, l, r) :: t => traverseTree(t ::: List(l, r), v +: result)
  }

  traverseTree(List(t), Vector())
}

treeToVector(t1) == Vector(1, 2, 3, 4, 5, 6, 7)
treeToVector(t2) == Vector(1, 7, 3, 4, 5, 6, 8)
treeToVector(Empty) == Vector()

def deleteDuplicatesB(t1: BinaryTree[Int], t2: BinaryTree[Int]): (BinaryTree[Int], BinaryTree[Int]) = {
  val v1 = treeToVector(t1)
  val v2 = treeToVector(t2)
  if (v1.length != v2.length) throw new Exception("Trees ought to be of the same shape!")

  def createUniqueTrees(index: Int): (BinaryTree[Int], BinaryTree[Int]) = {
    if (index > v1.length - 1) (Empty, Empty)
    else if (2 * index + 1 >= v1.length) {
      if (v1(index) == v2(index)) (Empty, Empty)
      else (Node(v1(index), Empty, Empty), Node(v2(index), Empty, Empty))
    }
    else {
      val lS = createUniqueTrees(2 * index + 1)
      val rS = createUniqueTrees(2 * index + 2)

      if (lS._1 == Empty && rS._1 == Empty) {
        if (v1(index) == v2(index)) (Empty, Empty)
        else (Node(v1(index), Empty, Empty), Node(v2(index), Empty, Empty))
      }
      else if (v1(index) == v2(index)) (Node(-1, lS._1, rS._1), Node(-1, lS._2, rS._2))
      else (Node(v1(index), lS._1, rS._1), Node(v2(index), lS._2, rS._2))
    }

  }

  createUniqueTrees(0)
}

deleteDuplicatesB(t1, t2) == expectedResult
deleteDuplicatesB(t3, t3) == (Empty, Empty)
deleteDuplicatesB(Empty, Empty) == (Empty, Empty)
//Zad4 (5pkt)
def from(value: Int): LazyList[Int] = value #:: from(value + 1)

def eachNElement[A](list: LazyList[A])(n: Int)(m: Int): LazyList[A] = {
  if (n <= 0 || m < 0) throw new IllegalArgumentException("Wrong interval given!")

  @scala.annotation.tailrec
  def createFilteredStream(list: LazyList[A])(result: LazyList[A])(currentIndex: Int): LazyList[A] = {
    if (currentIndex == m) result.reverse
    else if (list.isEmpty) throw new Exception("Stream is too short!")
    else if (currentIndex % n == 0) createFilteredStream(list.tail)(list.head #:: result)(currentIndex + 1)
    else createFilteredStream(list.tail)(result)(currentIndex + 1)
  }

  createFilteredStream(list)(list.empty)(0)
}

eachNElement(LazyList(5, 6, 3, 2, 1))(2)(3) == LazyList(5, 3)
eachNElement(LazyList(5, 6, 3, 2, 1))(2)(4) == LazyList(5, 3)
eachNElement(LazyList())(2)(0) == LazyList()
eachNElement(from(4))(2)(10) == LazyList(4, 6, 8, 10, 12)

//Zad5 (5pkt)
def ldzialanie[A](firstList: LazyList[A])(secondList: LazyList[A])(operator: (A, A) => A): LazyList[A] = (firstList, secondList) match {
  case (LazyList(), LazyList()) => LazyList()
  case (LazyList(), _) => secondList.head #:: ldzialanie(LazyList())(secondList.tail)(operator)
  case (_, LazyList()) => firstList.head #:: ldzialanie(firstList.tail)(LazyList())(operator)
  case _ => operator(firstList.head, secondList.head) #:: ldzialanie(firstList.tail)(secondList.tail)(operator)
}

ldzialanie(LazyList(0, 1, 2, 3))(LazyList())(_ - _) == LazyList(0, 1, 2, 3)
ldzialanie(LazyList(1, 2, 3))(LazyList(2, 3, 4, 5))(_ + _) == LazyList(3, 5, 7, 5)
ldzialanie(LazyList(1, -2, -3, 4))(LazyList(-1, 2, -3, 4, 5))(_ * _) == LazyList(-1, -4, 9, 16, 5)
ldzialanie(LazyList(2, 4, 6, 8))(LazyList(2, 2, 2, 2, 3))(_ / _) == LazyList(1, 2, 3, 4, 3)
ldzialanie(LazyList("Anna ", "Beata ", "Cecylia ", "Dorota "))(LazyList("Adam", "Bogdan", "Czeslaw", "Dominik"))(_ + _) == LazyList("Anna Adam", "Beata Bogdan", "Cecylia Czeslaw", "Dorota Dominik")
