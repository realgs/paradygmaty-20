import org.scalatest.flatspec.AnyFlatSpec

import scala.ExerciseOneAndTwo._
import scala.collection.immutable.{ListSet, Queue}

class ExerciseOneAndTwoTests extends AnyFlatSpec {
  "duplicate function" should "work properly if repetitions queue size is greater or equal than entry queue size" in {
    assert(duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(duplicate(Queue(5, 7, 9), Queue(0, 3, 3)) == Queue(7, 7, 7, 9, 9, 9))
    assert(duplicate(Queue(0, -2, 7, 2), Queue(1, 0, 1, 0, 50, 70, 200)) == Queue(0, 7))
    assert(duplicate(Queue("some", "text"), Queue(3, 0, 1, 2)) == Queue("some", "some", "some"))
  }

  "duplicate function" should "throw an IllegalArgumentException if repetitions queue size is less than entry queue size" in {
    assertThrows[IllegalArgumentException](duplicate(Queue(1, 2, 3), Queue(1, 2)))
    assertThrows[IllegalArgumentException](duplicate(Queue(5, 10, 100), Queue()))
  }

  "duplicate function" should "throw an IllegalArgumentException if repetitions queue contains at least one element with negative value in n first elements where n is size of the entry queue" in {
    assertThrows[IllegalArgumentException](duplicate(Queue(1, 2, 3), Queue(2, 2, -1, 10)))
    assertThrows[IllegalArgumentException](duplicate(Queue(0, -3, 12, 11), Queue(-10, 0, 0, 1)))
    assertThrows[IllegalArgumentException](duplicate(Queue(0, 1, -2, 3), Queue(1, 2, -2, 1)))
  }

  "duplicateWithoutDuplicates function" should "work properly if repetitions queue size is greater or equal than set size" in {
    assert(duplicateWithoutDuplicates(ListSet(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(duplicateWithoutDuplicates(ListSet(5, 7, 9), Queue(0, 3, 3)) == Queue(7, 7, 7, 9, 9, 9))
    assert(duplicateWithoutDuplicates(ListSet(0, -2, 7, 2), Queue(1, 0, 1, 0, 50, 70, 200)) == Queue(0, 7))
    assert(duplicateWithoutDuplicates(ListSet("some", "text"), Queue(3, 0, 1, 2)) == Queue("some", "some", "some"))
  }

  "duplicateWithoutDuplicates function" should "ignore duplicates in first collection and return desired result" in {
    assert(duplicateWithoutDuplicates(ListSet(1, 1, 1, 1, 2, 3, 2, 2, 1, 3, 2), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(duplicateWithoutDuplicates(ListSet(5, 7, 5, 7, 9), Queue(0, 3, 3)) == Queue(7, 7, 7, 9, 9, 9))
    assert(duplicateWithoutDuplicates(ListSet(0, -2, 0, 0, 0, 0, 0, -2,  7, 2, 7, 0, 2), Queue(1, 0, 1, 0, 50, 70, 200)) == Queue(0, 7))
    assert(duplicateWithoutDuplicates(ListSet("some", "some", "text"), Queue(3, 0, 1, 2)) == Queue("some", "some", "some"))
  }

  "duplicateWithoutDuplicates function" should "throw an IllegalArgumentException if repetitions queue size is less than first queue size" in {
    assertThrows[IllegalArgumentException](duplicateWithoutDuplicates(ListSet(1, 2, 3), Queue(1, 2)))
    assertThrows[IllegalArgumentException](duplicateWithoutDuplicates(ListSet(5, 10, 100), Queue()))
  }

  "duplicateWithoutDuplicates function" should "throw an IllegalArgumentException if repetitions queue contains at least one element with negative value in n first elements where n is size of the set" in {
    assertThrows[IllegalArgumentException](duplicateWithoutDuplicates(ListSet(1, 2, 3), Queue(2, 2, -1, 10)))
    assertThrows[IllegalArgumentException](duplicateWithoutDuplicates(ListSet(0, -3, 12, 11), Queue(-10, 0, 0, 1)))
    assertThrows[IllegalArgumentException](duplicateWithoutDuplicates(ListSet(0, 1, -2, 3), Queue(1, 2, -2, 1)))
  }
}
