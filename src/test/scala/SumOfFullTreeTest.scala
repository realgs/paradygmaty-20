import algorithms.{Empty, Node, SumOfFullTree}
import org.scalatest.FunSuite

class SumOfFullTreeTest extends FunSuite {
  test("sequentialSumTest") {
    val tree = Node(1, Node(2, Node(4,Empty,Empty),Node(5,Empty,Empty)),Node(3, Node(6,Empty,Empty),Node(7,Empty,Empty)))
    assert(SumOfFullTree.sequential(tree) == 28)
  }

  test("parallelSumTest") {
    val tree = Node(1, Node(2, Node(4,Empty,Empty),Node(5,Empty,Empty)),Node(3, Node(6,Empty,Empty),Node(7,Empty,Empty)))
    assert(SumOfFullTree.parallel(tree) == 28)
  }
}
