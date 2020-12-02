import L5Lab.{Empty, Point, duplicate, duplicateIgnoreDuplicates}
import org.scalatest.FunSuite

import scala.collection.immutable.Queue
import scala.collection.mutable.LinkedHashSet

class L5LabTest extends FunSuite {

  test("Task 1") {
    assert(duplicate(Queue(1, 2, 3, 4), Queue(1, 2, 3, 4)) == Queue(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
    assert(duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(duplicate(Queue(1), Queue(0)) == Queue())
    assert(duplicate(Queue(), Queue(2)) == Queue())
    assert(duplicate(Queue(1), Queue(-10)) == Queue())
    assert(duplicate(Queue('a', 'b', 'c'), Queue(0, 2)) == Queue('b', 'b'))
    assert(duplicate(Queue("abc"), Queue(3, -1)) == Queue("abc", "abc", "abc"))
  }

  test("Task 2") {
    assert(duplicateIgnoreDuplicates(LinkedHashSet(1, 2, 3, 1, 3, 3, 4), Queue(1, 2, 3, 4)) == Queue(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
    assert(duplicateIgnoreDuplicates(LinkedHashSet(), Queue(1, 2, 3, 4)) == Queue())
    assert(duplicateIgnoreDuplicates(LinkedHashSet(1, 2, 3, 4), Queue()) == Queue())
    assert(duplicateIgnoreDuplicates(LinkedHashSet(1, 2, 3, 4), Queue(1, 2, 3, 4)) == Queue(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
  }

  test("Task 3") {
    val testPoint = new Point(3, 4)
    val testEmpty = new Empty

    testPoint.printDebugName()
    println()
    testEmpty.printDebugName()
  }

  test("Task 4") {
    val testPoint = new Point(3, 4)
    val testEmpty = new Empty

    testPoint.printDebugVariables()
    println()
    testEmpty.printDebugVariables()
  }

  test("Task 5") {
    val testPoint = new Point(3, 4)
    val testEmpty = new Empty

    assert(testPoint.debugName == "Point")
    assert(testEmpty.debugName == "Empty")
  }
}
