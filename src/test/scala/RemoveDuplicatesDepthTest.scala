import org.scalatest.FunSuite

// testy do zadania 3 z wyszukiwaniem wglab (1 punkt)
class RemoveDuplicatesDepthTest extends  FunSuite{

  test("basicTest1") {
    val mockTree1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val mockTree2 = Node(1, Node(2, Node(4, Empty, Empty), Node(4, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))

    val (result1, result2) = Lista4.removeDuplicatesDepth(mockTree1, mockTree2)

    assert(result1 == Node(-1, Node(-1, Empty, Node(5, Empty, Empty)), Empty))
    assert(result2 == Node(-1, Node(-1, Empty, Node(4, Empty, Empty)), Empty))
  }

  test("basicTest2") {
    val mockTree1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val mockTree2 = Node(4, Node(2, Node(8, Empty, Empty), Node(9, Empty, Empty)), Node(5, Node(1, Empty, Empty), Node(7, Empty, Empty)))

    val (result1, result2) = Lista4.removeDuplicatesDepth(mockTree1, mockTree2)

    assert(result1 == Node(1, Node(-1, Node(4,Empty, Empty), Node(5, Empty, Empty)), Node(3,Node(6, Empty, Empty),Empty)))
    assert(result2 == Node(4, Node(-1, Node(8, Empty, Empty), Node(9, Empty, Empty)), Node(5, Node(1, Empty, Empty), Empty)))
  }

  test("allEqualElements") {
    val mockTree = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))

    assert(Lista4.removeDuplicatesDepth(mockTree, mockTree) == (Empty, Empty))
  }

  test("allDifferentElements") {
    val mockTree1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val mockTree2 = Node(9, Node(9, Node(9, Empty, Empty), Node(9, Empty, Empty)), Node(9, Node(9, Empty, Empty), Node(9, Empty, Empty)))

    assert(Lista4.removeDuplicatesDepth(mockTree1, mockTree2) == (mockTree1, mockTree2))
  }

  test("differentDepths") {
    assertThrows[IllegalArgumentException](
      Lista4.removeDuplicatesDepth(
        Lista4.generateRandomTree(4, 1, 2),
        Lista4.generateRandomTree(5, 3, 4)
      )
    )
  }

}
