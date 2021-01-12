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
    val firstTestTree = Node(10, Node(12, Node(6, Empty, Empty), Node(13, Empty, Empty)), Node(6, Node(1, Empty, Empty), Node(12, Empty, Empty)))
    val secondTestTree = Node(2, Node(7, Node(8, Empty, Empty), Node(7, Empty, Empty)), Node(2, Node(9, Empty, Empty), Node(9, Empty, Empty)))
    val resultTree = Node(8, Node(5, Node(-2, Empty, Empty), Node(6, Empty, Empty)), Node(4, Node(-8, Empty, Empty), Node(3, Empty, Empty)))
    assert(testFunction.diffTrees(firstTestTree, secondTestTree) == resultTree)
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

  @Test def testFunction4 = {
    assert(testFunction.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    assert(testFunction.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3))
    assert(testFunction.eachNElement(LazyList('h', 'e', 'j', 'k', 'a'), 2, 5) == LazyList('h', 'j', 'a'))
    assert(testFunction.eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 5, 0) == LazyList())
    assert(testFunction.eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 5, 8) == LazyList(1,6))

  }

  @Test def testFunction5 = {
    //testing int
    assert(testFunction.ldzialanie(LazyList(1,2,3), LazyList(2,3,4,5), '+') == LazyList(3,5,7,5))
    assert(testFunction.ldzialanie(LazyList(1,2,3), LazyList(2,3,4,5), '-') == LazyList(-1,-1,-1,5))
    assert(testFunction.ldzialanie(LazyList(1,2,3), LazyList(2,3,4,5), '*') == LazyList(2,6,12,5))

    //testing double
    assert(testFunction.ldzialanieD(LazyList(1,2,3), LazyList(2,3,4,5), '+') == LazyList(3,5,7,5))
    assert(testFunction.ldzialanieD(LazyList(1,2,3), LazyList(2,3,4,5), '-') == LazyList(-1,-1,-1,5))
    assert(testFunction.ldzialanieD(LazyList(1,2,3), LazyList(2,3,4,5), '*') == LazyList(2,6,12,5))
  }
}
