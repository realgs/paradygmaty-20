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
    assertResult(Node(26, Node(20, Node(14, Empty, Empty), Node(22, Empty, Empty)), Node(11, Node(16, Empty, Empty), Node(27, Empty, Empty)))) {
      /**       26
       *   20        11
       * 14  22    16  27
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
      /**       27
       *    21      28
       * 31  33    35  32
       */
      val second = generateTree(3, rand, 10, 50)

      /**       42
       *    45       48
       * 49  25    24  18
       */

      subtractTrees(first, second)

      /**       -15
       *    -24      -20
       * -18  8    11  14
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

  test("removeDuplicatesDFS") {
    val first_tree = Node(4, Node(8, Node(7, Empty, Empty), Node(16, Empty, Empty)), Node(9, Node(3, Empty, Empty), Node(1, Empty, Empty)))
    val second_tree = Node(8, Node(8, Node(7, Empty, Empty), Node(10, Empty, Empty)), Node(9, Node(3, Empty, Empty), Node(2, Empty, Empty)))
    val expected = (Node(4, Node(-1, Empty, Node(16, Empty, Empty)), Node(-1, Empty, Node(1, Empty, Empty))),
      Node(8, Node(-1, Empty, Node(10, Empty, Empty)), Node(-1, Empty, Node(2, Empty, Empty))))

    assertResult(expected) {
      removeDuplicatesDFS(first_tree, second_tree)
    }

    assertThrows[IllegalArgumentException] {
      // Not full
      removeDuplicatesDFS(Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty)),
        Node(3, Node(4, Empty, Empty), Empty))
    }

    assertThrows[IllegalArgumentException] {
      // Not same depth
      removeDuplicatesDFS(Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty)),
        Node(3, Node(4, Node(5, Empty, Empty), Node(7, Empty, Empty)), Node(1, Node(10, Empty, Empty), Node(12, Empty, Empty))))
    }
  }

  test("eachNElement") {
    assertResult(LazyList()) {
      eachNElement(LazyList(1, 2, 3), 1, 0)
    }
    assertResult(LazyList(1)) {
      eachNElement(LazyList(1, 2, 3), 5, 3)
    }
    assertResult(LazyList(1)) {
      eachNElement(LazyList(1, 2, 3), 1, 1)
    }
    assertResult(LazyList(5, 3)) {
      eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3)
    }
    assertResult(LazyList(5, 3)) {
      eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4)
    }
    assertResult(LazyList(5, 6, 3, 2, 1)) {
      eachNElement(LazyList(5, 6, 3, 2, 1), 1, 5)
    }
    assertResult(LazyList(5, 2)) {
      eachNElement(LazyList(5, 6, 3, 2, 1), 3, 4)
    }
    assertResult(LazyList(1,3,5,7,9,11)) {
      eachNElement(LazyList.from(1), 2, 11)
    }
    assertThrows[IllegalArgumentException] {
      eachNElement(LazyList(5, 6, 3, 2, 1), 0, 3)
    }
    assertThrows[IllegalArgumentException] {
      eachNElement(LazyList(5, 6, 3, 2, 1), 2, -2)
    }

  }

  test("lazyAction") {
    assertResult(LazyList()) {
      lazyAction(LazyList(), LazyList(), '*')
    }
    assertResult(LazyList(1)) {
      lazyAction(LazyList(1), LazyList(0), '+')
    }
    assertResult(LazyList(1, 2, 3)) {
      lazyAction(LazyList(), LazyList(1, 2, 3), '+')
    }
    assertResult(LazyList(3, 5, 7, 5)) {
      lazyAction(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+')
    }
    assertResult(LazyList(4, 16, 32, 64)) {
      lazyAction(LazyList(2, 2, 2, 2), LazyList(2, 8, 16, 32), '*')
    }
    assertResult(LazyList(5, 7, 9, 3)) {
      lazyAction(LazyList(12, 14, 19, 4), LazyList(7, 7, 10, 1), '-')
    }
    assertResult(LazyList(2, 4, 8, 16)) {
      lazyAction(LazyList(18, 16, 64, 32), LazyList(9, 4, 8, 2), '/')
    }
    assertThrows[NotImplementedError] {
      lazyAction(LazyList(1, 2, 3), LazyList(1, 2, 3), '%')
    }

  }
}
