import org.scalatest.FunSuite

class TreesDifferenceTest extends FunSuite {

  test("properResult") {
    val mockTree = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val resultTree = Lista4.treesDifference(mockTree, mockTree)

    assert(Utils.breadthSearch(resultTree) == List(0, 0, 0, 0, 0, 0, 0))
  }

  test("twoEmptyTrees") {
    assert(Lista4.treesDifference(Empty, Empty) == Empty)
  }

  test("differentDepths") {
    assertThrows[IllegalArgumentException](
      Lista4.treesDifference(
        Lista4.generateRandomTree(4, 1, 2),
        Lista4.generateRandomTree(5, 3, 4)
      )
    )
  }

}
