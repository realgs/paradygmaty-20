import L4Lab.{Empty, Node, add, divide, eachNElement, generateTree, isTreeFull, lOperation, multiply, removeDuplicatesDepth, subtract, subtractTrees}
import org.scalatest.FunSuite

class L4LabTest extends FunSuite {

  test("Task 1") {
    assert(isTreeFull(generateTree(3, 1, 10)))
    assert(isTreeFull(generateTree(4, 100, 101)))
    assert(isTreeFull(generateTree(10, 10, 11)))
    assert(generateTree(0, 1, 10) == Empty)
    assertThrows[IllegalArgumentException] {
      generateTree(-10, 1, 10)
    }
    assertThrows[IllegalArgumentException] {
      generateTree(3, 10, 1)
    }
  }

  test("Task 2") {
    val firstTree = Node(10, Node(5, Empty, Empty), Node(4, Empty, Empty))
    val secondTree = Node(5, Node(5, Empty, Empty), Node(-10, Empty, Empty))
    val secondTreeException = Node(5, Node(5, Empty, Node(10, Node(5, Empty, Empty), Node(4, Empty, Empty))), Node(-10, Empty, Node(10, Node(5, Empty, Empty), Node(4, Empty, Empty))))
    assert(subtractTrees(firstTree, secondTree) == Node(5, Node(0, Empty, Empty), Node(14, Empty, Empty)))
    assertThrows[IllegalArgumentException] {
      subtractTrees(firstTree, secondTreeException)
    }
  }

  test("Task 3") {
    val testTreeNormal1 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val testTreeNormal2 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val expectedTestTreeNormal1 = Node(-1, Node(-1, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Empty, Empty))
    val expectedTestTreeNormal2 = Node(-1, Node(-1, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Empty, Empty))

    assert(
      removeDuplicatesDepth(testTreeNormal1, testTreeNormal2) == (
        expectedTestTreeNormal1,
        expectedTestTreeNormal2
      )
    )

    val testTreeAllDifferent1 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val testTreeAllDifferent2 = Node(2, Node(3, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Node(7, Empty, Empty), Node(8, Empty, Empty)))
    val expectedTestTreeAllDifferent1 = Node(1, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(5, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val expectedTestTreeAllDifferent2 = Node(2, Node(3, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(6, Node(7, Empty, Empty), Node(8, Empty, Empty)))

    assert(
      removeDuplicatesDepth(testTreeAllDifferent1, testTreeAllDifferent2) == (
        expectedTestTreeAllDifferent1,
        expectedTestTreeAllDifferent2
      )
    )

    val testTreeAllEqual1 = Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
    val testTreeAllEqual2 = Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)))
    val expectedTestTreeAllEqual1 = Empty
    val expectedTestTreeAllEqual2 = Empty

    assert(
      removeDuplicatesDepth(testTreeAllEqual1, testTreeAllEqual2) == (
        expectedTestTreeAllEqual1,
        expectedTestTreeAllEqual2
      )
    )

    assertThrows[IllegalArgumentException](
      removeDuplicatesDepth(
        generateTree(1, 1, 2),
        Node(1, Node(1, Node(1, Empty, Empty), Node(1, Empty, Empty)), Node(1, Node(1, Empty, Empty), Empty))
      )
    )

    assertThrows[IllegalArgumentException](
      removeDuplicatesDepth(
        generateTree(1, 1, 2),
        generateTree(2, 1, 2)
      )
    )

  }

  test("Task 4") {
    assert(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3))
    assert(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    assert(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4, 10) == LazyList(1, 5, 9))
    assert(eachNElement(LazyList(5, 6, 3, 2, 1), 1, 5) == LazyList(5, 6, 3, 2, 1))
    assertThrows[IllegalArgumentException] {
      eachNElement(LazyList(5, 6, 3, 2, 1), 0, 4).force
    }
    assertThrows[IllegalArgumentException] {
      eachNElement(LazyList(5, 6, 3, 2, 1), 2, -10).force
    }
  }

  test("Task 5") {
    assert(lOperation(LazyList(1, 2, 3), LazyList(4, 5), add) == LazyList(5, 7, 3))
    assert(lOperation(LazyList(1, 2, 3), LazyList(4, 1.5, -100), subtract) == LazyList(-3, 0.5, 103))
    assert(lOperation(LazyList(1, 2, 3), LazyList(4, 5), multiply) == LazyList(4, 10, 3))
    assert(lOperation(LazyList(10, 20, 30), LazyList(1, 10, 0.5), divide) == LazyList(10, 2, 60))
    assert(lOperation(LazyList(), LazyList(), add) == LazyList())
    assert(lOperation(LazyList(1, 2), LazyList(), subtract) == LazyList(1, 2))
    assertThrows[IllegalArgumentException] {
      lOperation(LazyList(1), LazyList(0), divide).force
    }
  }

}
