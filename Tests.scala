import org.junit.jupiter.api.{Assertions, Test}

class Tests {
  val testFunction = new Functions

  @Test def testFunction1 = {
    //testing if tree depth is correct
    assert(testFunction.checkDepth(testFunction.generateTree(1, 1, 10)) == 1)
    assert(testFunction.checkDepth(testFunction.generateTree(5, 1, 10)) == 5)
    assert(testFunction.checkDepth(testFunction.generateTree(10, 2, 3)) == 10)

    //testing incorrect arguments
    Assertions.assertThrows(classOf[Exception], () => testFunction.generateTree(-1, 1, 5))
    Assertions.assertThrows(classOf[Exception], () => testFunction.generateTree(4, 4, 2))
    Assertions.assertThrows(classOf[Exception], () => testFunction.checkDepth(testFunction.generateTree(0, 1, 10)))

    //testing if tree is full
    assert(testFunction.isTreeFull(testFunction.generateTree(3, 1, 5)))
    assert(testFunction.isTreeFull(testFunction.generateTree(7, 1, 3)))
    assert(testFunction.isTreeFull(testFunction.generateTree(1, 1, 3)))

  }

  @Test def testFunction2 = {
    assert(testFunction.diffTrees(testFunction.firstTestTree, testFunction.secondTestTree) == testFunction.resultTree)
  }

  @Test def testFunction3 = {
    val tree1 = Node(1, Node(2, Node(3, Empty, Empty), Node(1, Empty, Empty)), Node(4, Node(4, Empty, Empty), Node(2, Empty, Empty)))
    val tree2 = Node(2, Node(2, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(4, Node(4, Empty, Empty), Node(2, Empty, Empty)))
    val result = (Node(1, Node(-1, Empty, Node(1, Empty, Empty)), Empty),
      Node(2, Node(-1, Empty, Node(4, Empty, Empty)), Empty))

    //testing DFS
    assert(testFunction.deleteDuplicatesDFS(tree1, tree2) == result)

    //testing BFS
    assert(testFunction.deleteDuplicatesBFS(tree1, tree2) == result)
  }


}
