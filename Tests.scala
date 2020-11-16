import org.junit.jupiter.api.{Assertions, Test}

class Tests {
  val testFunction = new Functions

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  @Test def testFunction1 = {
    //testing if tree depth is correct
    assert(testFunction.checkDepth(testFunction.generateTree(1,1,10)) == 1)
    assert(testFunction.checkDepth(testFunction.generateTree(5,1,10)) == 5)
    assert(testFunction.checkDepth(testFunction.generateTree(10,2,3)) == 10)

    //testing incorrect arguments
    Assertions.assertThrows(classOf[Exception],() => testFunction.generateTree(-1, 1, 5))
    Assertions.assertThrows(classOf[Exception],() => testFunction.generateTree(4, 4, 2))

    //testing if tree is full
    assert(testFunction.isTreeFull(testFunction.generateTree(3, 1, 5)))
    assert(testFunction.isTreeFull(testFunction.generateTree(7, 1, 3)))
  }

  @Test def testFunction2 = {
    assert(testFunction.diffTrees(testFunction.firstTestTree, testFunction.secondTestTree) == testFunction.resultTree)

  }
}
