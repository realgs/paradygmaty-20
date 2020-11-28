import org.junit.jupiter.api.Test

import scala.collection.immutable.Queue

class Tests {
  val testFunction = new Functions

  @Test
  def testFunction1 = {
    assert(testFunction.duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(testFunction.duplicate(Queue(), Queue(1,2,3)) == Queue())
    assert(testFunction.duplicate(Queue('b', 'a','b'), Queue(0, 0, 3)) == Queue('b', 'b', 'b'))
  }

  @Test
  def testFunction2 = {
    assert(testFunction.duplicateWithNoReps(Set(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(testFunction.duplicateWithNoReps(Set(), Queue(1,2,3)) == Queue())
    assert(testFunction.duplicateWithNoReps(Set('b', 'a','c'), Queue(0, 0, 3)) == Queue('c', 'c', 'c'))
    assert(testFunction.duplicateWithNoReps(Set(1,1,2,3), Queue(3,3,3,3)) == Queue(1,1,1,2,2,2,3,3,3))
    assert(testFunction.duplicateWithNoReps(Set('a', 'n', 'n', 'a'), Queue(1,2,3,4)) == Queue('a', 'n', 'n'))
  }
}
