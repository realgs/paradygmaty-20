import org.scalatest.FunSuite

class zad02Test extends FunSuite {
  test("empty trees subtraction") {
    val a = Empty
    val b = Empty
    val expected = Empty
    assert(zad02.subtractTrees(a, b) == expected)
  }

  test("one node trees subtraction") {
    val a = Node(15, Empty, Empty)
    val b = Node(10, Empty, Empty)
    val expected = Node(5, Empty, Empty)
    assert(zad02.subtractTrees(a, b) == expected)
  }

  test("medium trees subtraction") {
    val a = Node(15,
      Node(17, Node(2, Empty, Empty), Node(4, Empty, Empty)),
      Node(7, Node(1, Empty, Empty), Node(14, Empty, Empty)))
    val b = Node(4,
      Node(3, Node(12, Empty, Empty), Node(2, Empty, Empty)),
      Node(11, Node(9, Empty, Empty), Node(1, Empty, Empty)))
    val expected = Node(11,
      Node(14, Node(-10, Empty, Empty), Node(2, Empty, Empty)),
      Node(-4, Node(-8, Empty, Empty), Node(13, Empty, Empty)))
    assert(zad02.subtractTrees(a, b) == expected)
  }

  test("diferent depth trees subtraction") {
    val a = Node(15, Empty, Empty)
    val b = Node(10, Node(1, Empty, Empty), Node(2, Empty, Empty))
    assertThrows[IllegalArgumentException] {zad02.subtractTrees(a, b)}
  }
}
