import org.scalatest.FunSuite

class TreesTest extends FunSuite{

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

  test("treeToList.test"){
    val tree = Trees.generateTree(2, 1, 1)
    val list = Trees.treeToList(tree)
    assert(list === List(1,1,1))
  }
  test("ParallelTreeToList.test"){
    val tree = Trees.generateTree(2, 1, 1)
    val list = Trees.parallelTreeToList(tree)
    assert(list === List(1,1,1))
  }

  test("ParallelTreeToList.test_long"){
    val tree = Trees.generateTree(4, 1, 1)
    val list = Trees.parallelTreeToList(tree)
    assert(list === List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
  }
}
