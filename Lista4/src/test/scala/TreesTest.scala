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
  test("createTree.height_2"){
    assert(Trees.generateTree(2, 1, 1) === Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty)))
  }
  test("createTree.height_-2"){
    assertThrows[Exception](Trees.generateTree(-2,5,8))
  }
  test("createTree.EmptyTree"){
    val tree = Trees.generateTree(0, 10, 22)
    assert(Trees.getHeight(tree) === 0)
    assert(Trees.isTreeFull(tree))
  }

  //task 2 tests
  test("subtractInTrees.height_3_val_random_1"){
    assert(Trees.subtractInTrees(Trees.generateTree(3,1,1),Trees.generateTree(3,1,1) ) ===
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
    assert(Trees.subtractInTrees(Trees.generateTree(0,1,2),Trees.generateTree(0,1,2) ) === Trees.Empty)
  }
  test("subtractInTrees.height&fullTest"){
    val tree = Trees.subtractInTrees(Trees.generateTree(5, 6, 20), Trees.generateTree(5, 20, 60))
    assert(Trees.getHeight(tree) === 5)
    assert(Trees.isTreeFull(tree))
  }

  //task 3 DFS tests
  test("removeSameValuesDFS.1"){
    val tree1 = Trees.Node(1, Trees.Node(2, Trees.Node(4, Trees.Empty, Trees.Empty), Trees.Node(5, Trees.Empty, Trees.Empty)), Trees.Node(3, Trees.Node(6, Trees.Empty, Trees.Empty), Trees.Node(7, Trees.Empty, Trees.Empty)))
    val tree2 = Trees.Node(1, Trees.Node(3, Trees.Node(4, Trees.Empty, Trees.Empty), Trees.Node(6, Trees.Empty, Trees.Empty)), Trees.Node(3, Trees.Node(6, Trees.Empty, Trees.Empty), Trees.Node(7, Trees.Empty, Trees.Empty)))
    val (result1, result2) = Trees.removeSameValuesDFS(tree1, tree2)
    assert(result1 == Trees.Node(-1, Trees.Node(2, Trees.Empty, Trees.Node(5, Trees.Empty, Trees.Empty)), Trees.Empty))
    assert(result2 == Trees.Node(-1, Trees.Node(3, Trees.Empty, Trees.Node(6, Trees.Empty, Trees.Empty)), Trees.Empty))
  }
  test("removeSameValuesDFS.2"){
    val tree1 = Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty))
    val tree2 = Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty))
    val (result1, result2) = Trees.removeSameValuesDFS(tree1, tree2)
    assert(result1 == Trees.Empty)
    assert(result2 == Trees.Empty)
  }
  test("removeSameValuesDFS.emptyTrees"){
    assert(Trees.removeSameValuesDFS(Trees.Empty, Trees.Empty) === (Trees.Empty, Trees.Empty))
  }
  test("removeSameValuesDFS.unequalTrees"){
    assertThrows[Exception](Trees.removeSameValuesDFS(Trees.generateTree(3, 5, 8), Trees.generateTree(2, 6, 9)))
  }

  //task 3 BFS tests
  test("removeSameValuesBFS.1"){
    val tree1 = Trees.Node(1, Trees.Node(2, Trees.Node(4, Trees.Empty, Trees.Empty), Trees.Node(5, Trees.Empty, Trees.Empty)), Trees.Node(3, Trees.Node(6, Trees.Empty, Trees.Empty), Trees.Node(7, Trees.Empty, Trees.Empty)))
    val tree2 = Trees.Node(1, Trees.Node(3, Trees.Node(4, Trees.Empty, Trees.Empty), Trees.Node(6, Trees.Empty, Trees.Empty)), Trees.Node(3, Trees.Node(6, Trees.Empty, Trees.Empty), Trees.Node(7, Trees.Empty, Trees.Empty)))
    val (result1, result2) = Trees.removeSameValuesBFS(tree1, tree2)
    assert(result1 == Trees.Node(-1, Trees.Node(2, Trees.Empty, Trees.Node(5, Trees.Empty, Trees.Empty)), Trees.Empty))
    assert(result2 == Trees.Node(-1, Trees.Node(3, Trees.Empty, Trees.Node(6, Trees.Empty, Trees.Empty)), Trees.Empty))
  }
  test("removeSameValuesBFS.2"){
    val tree1 = Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty))
    val tree2 = Trees.Node(1, Trees.Node(1, Trees.Empty, Trees.Empty), Trees.Node(1, Trees.Empty, Trees.Empty))
    val (result1, result2) = Trees.removeSameValuesBFS(tree1, tree2)
    assert(result1 == Trees.Empty)
    assert(result2 == Trees.Empty)
  }
  test("removeSameValuesBFS.emptyTrees"){
    assert(Trees.removeSameValuesBFS(Trees.Empty, Trees.Empty) === (Trees.Empty, Trees.Empty))
  }
  test("removeSameValuesBFS.unequalTrees"){
    assertThrows[Exception](Trees.removeSameValuesBFS(Trees.generateTree(3, 5, 8), Trees.generateTree(2, 6, 9)))
  }

}
