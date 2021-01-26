import org.scalatest.FunSuite

class zad03Test extends FunSuite {
  test("[depth] empty trees duplicate removal") {
    val a = Empty
    val b = Empty
    val (actualA, actualB) = zad03.removeDuplicatesDepth(a, b)
    assert(actualA == Empty)
    assert(actualB == Empty)
  }

  test("[depth] one node trees duplicate removal") {
    val a = Node(1, Empty, Empty)
    val b = Node(1, Empty, Empty)
    val (actualA, actualB) = zad03.removeDuplicatesDepth(a, b)
    assert(actualA == Empty)
    assert(actualB == Empty)
  }

  test("[depth] medium trees duplicate removal") {
    val a = Node(3, Node(4, Node(1, Empty, Empty), Node(2, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty)))
    val b = Node(1, Node(4, Node(7, Empty, Empty), Node(9, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty)))
    val (actualA, actualB) = zad03.removeDuplicatesDepth(a, b)
    assert(actualA == Node(3, Node(-1, Node(1, Empty, Empty), Node(2, Empty, Empty)), Empty))
    assert(actualB == Node(1, Node(-1, Node(7, Empty, Empty), Node(9, Empty, Empty)), Empty))
  }

  test("[depth] different depth trees duplicate removal") {
    val a = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    val b = Node(3, Empty, Empty)
    assertThrows[IllegalArgumentException] {zad03.removeDuplicatesDepth(a, b)}
  }

  test("[breadth] empty trees duplicate removal") {
    val a = Empty
    val b = Empty
    val (actualA, actualB) = zad03.removeDuplicatesBreadth(a, b)
    assert(actualA == Empty)
    assert(actualB == Empty)
  }

  test("[breadth] one node trees duplicate removal") {
    val a = Node(1, Empty, Empty)
    val b = Node(1, Empty, Empty)
    val (actualA, actualB) = zad03.removeDuplicatesBreadth(a, b)
    assert(actualA == Empty)
    assert(actualB == Empty)
  }

  test("[breadth] medium trees duplicate removal") {
    val a = Node(3, Node(4, Node(1, Empty, Empty), Node(2, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty)))
    val b = Node(1, Node(4, Node(7, Empty, Empty), Node(9, Empty, Empty)), Node(9, Node(5, Empty, Empty), Node(6, Empty, Empty)))
    val (actualA, actualB) = zad03.removeDuplicatesBreadth(a, b)
    assert(actualA == Node(3, Node(-1, Node(1, Empty, Empty), Node(2, Empty, Empty)), Empty))
    assert(actualB == Node(1, Node(-1, Node(7, Empty, Empty), Node(9, Empty, Empty)), Empty))
  }

  test("[breadth] different depth trees duplicate removal") {
    val a = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    val b = Node(3, Empty, Empty)
    assertThrows[IllegalArgumentException] {zad03.removeDuplicatesBreadth(a, b)}
  }
}
