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
}
