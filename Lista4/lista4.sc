//Jakub Kochański
//Zad1 (3pkt)
sealed trait BinaryTree[+A]

case object Empty extends BinaryTree[Nothing]

case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

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