import org.scalatest.FunSuite

class TreesTest extends FunSuite {

  //task 1 tests
  test("createTree.height_0"){
    val tree = Trees.generateTree(0, 1, 2)
    assert(Trees.getHeight(tree) === 0)
    assert(Trees.isTreeFull(tree))
  }
  test("createTree.height_5"){
    val tree = Trees.generateTree(5, 10, 22)
    assert(Trees.getHeight(tree) === 5)
    assert(Trees.isTreeFull(tree))
  }
  test("createTree.height_1"){
    assert(Trees.generateTree(1, 1, 2) === Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty)))
  }
  test("createTree.height_-2"){
    assertThrows[Exception](Trees.generateTree(-2,5,8))
  }
  test("createTree.EmptyTree"){
    val tree = Trees.generateTree(-1, 10, 22)
    assert(Trees.getHeight(Trees.Empty) === -1)
    assert(Trees.isTreeFull(Trees.Empty))
  }

  //task 2 tests
  test("subtractInTrees.height_2_val_random_1"){
    assert(Trees.subtractInTrees(Trees.generateTree(2,1,2),Trees.generateTree(2,1,2) ) ===
      Trees.Node(0, Trees.Node(0, Trees.Node(0, Trees.Empty, Trees.Empty), Trees.Node(0, Trees.Empty, Trees.Empty)), Trees.Node(0, Trees.Node(0, Trees.Empty, Trees.Empty), Trees.Node(0, Trees.Empty, Trees.Empty))))
  }
  test("subtractInTrees.height_1_val_given"){
    assert(Trees.subtractInTrees(Trees.Node(5, Trees.Node(2, Trees.Empty, Trees.Empty), Trees.Node(6, Trees.Empty, Trees.Empty)), Trees.Node(2, Trees.Node(7, Trees.Empty, Trees.Empty), Trees.Node(6, Trees.Empty, Trees.Empty))) ===
      Trees.Node(3, Trees.Node(-5, Trees.Empty, Trees.Empty), Trees.Node(0, Trees.Empty, Trees.Empty)))
  }
  test("subtractionInTrees.unequalHeight"){
    assertThrows[Exception](Trees.subtractInTrees(Trees.generateTree(2, 1, 5), Trees.generateTree(3, 1, 4)))
  }
  test("subtractInTrees.emptyTrees"){
    assert(Trees.subtractInTrees(Trees.generateTree(-1,1,2),Trees.generateTree(-1,1,2) ) === Trees.Empty)
  }

}
