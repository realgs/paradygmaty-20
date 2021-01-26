import org.scalatest.FunSuite

class zad01Test extends FunSuite {
  test("zad01") {
    assert(isTreeValid(zad01.createFullBinaryTree(1, 1, 10), 1, 1, 10))
    assert(isTreeValid(zad01.createFullBinaryTree(0, 1, 2), 0, 1, 2))
    assert(isTreeValid(zad01.createFullBinaryTree(5, 1, 100), 5, 1, 100))
    assertThrows[IllegalArgumentException] {zad01.createFullBinaryTree(-10, 1, 2)}
    assertThrows[IllegalArgumentException] {zad01.createFullBinaryTree(1, -10, 2)}
    assertThrows[IllegalArgumentException] {zad01.createFullBinaryTree(1, 10, 5)}
  }

  private def isTreeValid(tree: BinaryTree[Int], depth: Int, left: Int, right: Int): Boolean = {
    (tree, depth) match {
      case (Empty, 0) => true
      case (Node(_, _, _), 0) => false
      case (Empty, _) => false
      case (Node(x, leftSubtre, rightSubtree), _) => (
        x >= left && x <= right
          && isTreeValid(leftSubtre, depth - 1, left, right)
          && isTreeValid(rightSubtree, depth - 1, left, right))
    }
  }

  test("isTreeValid") {
    val t1 = Node(1, Empty, Empty)
    assert(isTreeValid(t1, 1, 1, 2))
    val t2 = Node(-1, Node(1, Empty, Empty), Node(1, Empty, Empty))
    assert(!isTreeValid(t2, 2, 1, 2))
    val t3 = Empty
    assert(isTreeValid(t3, 0, 0, 0))
  }
}
