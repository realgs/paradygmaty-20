import lab05.Functions
import org.scalatest.FunSuite

import scala.collection.immutable.Queue

class FunctionTest extends FunSuite {

  test("Duplicate test") {
    assert(Functions.duplicate(Queue(1, 2, 3), Queue(1, 2, 3)) == Queue(1, 2, 2, 3, 3, 3))
    assert(Functions.duplicate(Queue(1), Queue(2, 2, 2)) == Queue(1, 1))
    assert(Functions.duplicate(Queue('a', 'b', 'c'), Queue(2, 1, 0)) == Queue('a', 'a', 'b'))
    assert(Functions.duplicate(Queue(1, 2, 3), Queue()) == Queue())
    assert(Functions.duplicate(Queue(), Queue(1, 2, 3)) == Queue())
    assert(Functions.duplicate(Queue(), Queue()) == Queue())
  }

  test("Duplicate without repetitions test") {
    assert(Functions.duplicateWithoutRepetitions(Set(5, 5, 5, 5), Queue(2, 2, 2, 2)) == Queue(5, 5))
    assert(Functions.duplicateWithoutRepetitions(Set("ala", "ala", "ewa"), Queue(1, 2, 3)) == Queue("ala", "ewa", "ewa"))
    assert(Functions.duplicateWithoutRepetitions(Set(5, 5, 1, 2), Queue(2, 3, 4, 5)) == Queue(5, 5, 1, 1, 1, 2, 2, 2, 2))
    assert(Functions.duplicateWithoutRepetitions(Set(1, 1, 1), Queue()) == Queue())
    assert(Functions.duplicateWithoutRepetitions(Set(), Queue(1, 1, 1)) == Queue())
    assert(Functions.duplicateWithoutRepetitions(Set(), Queue()) == Queue())
  }

}
