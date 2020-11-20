import DataStructures._
import org.scalatest._
import scala.util.Random

class DataStructuresTest extends funsuite.AnyFunSuite {
  test("generateTree") {
    assertResult(Empty) {
      generateTree(0, new Random(100), 1, 100)
    }
    assertResult(Node(16, Empty, Empty)) {
      generateTree(1, new Random(100), 1, 100)
    }
    assertResult(Node(26,Node(20,Node(14,Empty,Empty),Node(22,Empty,Empty)),Node(11,Node(16,Empty,Empty),Node(27,Empty,Empty)))) {
      /**         26
       *     20        11
       *   14  22    16  27
       */
      generateTree(3, new Random(100), 10, 30)
    }
    assertThrows[IllegalArgumentException] {
      generateTree(-1, new Random(), 1, 10)
    }
    assertThrows[IllegalArgumentException] {
      generateTree(3, new Random(), 10, 1)
    }
    assertThrows[IllegalArgumentException] {
      generateTree(3, new Random(), -5, 10)
    }
  }

  test("subtractTrees") {

    assertResult(Empty) {
      subtractTrees(Empty, Empty)
    }

    assertResult(Node(101, Empty, Empty)) {
      val rand = new Random(123)
      val first = generateTree(1, rand, 100, 200)
      val second = generateTree(1, rand, 50, 100)
      subtractTrees(first, second)
    }


    assertResult(Node(-15, Node(-24, Node(-18, Empty, Empty), Node(8, Empty, Empty)), Node(-20, Node(11, Empty, Empty), Node(14, Empty, Empty)))) {
      val rand = new Random(123)
      val first = generateTree(3, rand, 10, 50)
      /**         27
       *     21        28
       *   31  33    35  32
       */
      val second = generateTree(3, rand, 10, 50)
      /**         42
       *     45        48
       *   49  25    24  18
       */

      subtractTrees(first, second)
      /**        -15
       *     -24      -20
       *   -18  8    11  14
       */
    }

    assertThrows[IllegalArgumentException] {
      // Different depth
      val rand = new Random(123)
      val first = generateTree(4, rand, 100, 200)
      val second = generateTree(3, rand, 50, 100)
      subtractTrees(first, second)
    }

    assertThrows[IllegalArgumentException] {
      // Not full tree
      val first = Node(1, Node(2, Empty, Empty), Empty)
      val second = Node(0, Node(0, Empty, Empty), Node(0, Empty, Empty))
      subtractTrees(first, second)
    }
  }

}
