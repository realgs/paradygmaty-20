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
  test("createTree.height_-1"){
    assertThrows[Exception](Trees.generateTree(-1,5,8))
  }

}
