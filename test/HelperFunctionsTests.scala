import org.scalatest.flatspec.AnyFlatSpec

import scala.trees.{Empty, Node}
import scala.helper.HelperFunctions._

class HelperFunctionsTests extends AnyFlatSpec {
  "treeTest function" should "test check if all node's values are in given range correctly" in {
    val tree = Node(1, Node(2, Node(3, Empty, Empty), Node(5, Empty, Empty)), Node(6, Empty, Empty))

    assert(treeTest(tree, 1, 10))
    assert(!treeTest(tree, 1, 5))
  }

  "depth function" should "return correct depth" in {
    val tree = Node(1, Node(2, Node(3, Empty, Empty), Node(5, Empty, Empty)), Node(6, Empty, Empty))

    assert(depth(tree) == 3)
  }

  "isAFullTree function" should "give correct results" in {
    val t3 = Node(1, Node(2, Node(4, Node(8, Empty, Empty), Node(9, Empty, Empty)), Node(5, Node(10, Empty, Empty), Node(11, Empty, Empty))), Node(3, Node(6, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(7, Node(14, Empty, Empty), Empty)))
    val t4 = Node(0, Node(1, Node(3, Node(7, Empty, Empty), Node(8, Empty, Empty)), Node(4, Node(9, Empty, Empty), Node(10, Empty, Empty))), Node(2, Node(6, Node(11, Empty, Empty), Node(12, Empty, Empty)), Node(7, Node(14, Empty, Empty), Node(15, Empty, Empty))))

    assert(!isAFullTree(t3))
    assert(isAFullTree(t4))
    assert(isAFullTree(Empty))
  }
}
