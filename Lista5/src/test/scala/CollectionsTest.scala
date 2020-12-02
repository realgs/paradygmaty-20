import scala.collection.immutable.Queue
import org.scalatest.FunSuite

class CollectionsTest extends FunSuite{

  //Zad 1 tests
  test("duplicate.bothEmpty") {
    assert(Collections.duplicate(Queue(), Queue()) == Queue())
  }
  test("duplicate.repetitionsEmpty") {
    assert(Collections.duplicate(Queue(1, 2, 3), Queue()) == Queue())
  }
  test("duplicate.elementsEmpty") {
    assert(Collections.duplicate(Queue(), Queue(1, 2, 3)) == Queue())
  }
  test("duplicate.equalSizes") {
    assert(Collections.duplicate(Queue(1, 2, 3), Queue(3, 2, 1)) == Queue(1,1,1,2,2,3))
  }
  test("duplicate.lessRepetitions") {
    assert(Collections.duplicate(Queue(1, 2, 3), Queue(3, 2)) == Queue(1,1,1,2,2))
  }
  test("duplicate.moreRepetitions") {
    assert(Collections.duplicate(Queue(1, 2, 3), Queue(3, 2, 1, 4)) == Queue(1,1,1,2,2,3))
  }
  test("duplicate.negativeRepetitions") {
    assert(Collections.duplicate(Queue(1, 2, 3), Queue(3, -2, 1)) == Queue(1,1,1,3))
  }
  test("duplicate.equalSizes_String") {
    assert(Collections.duplicate(Queue("Ala", "ma", "kota"), Queue(3, 2, 1)) == Queue("Ala","Ala","Ala","ma","ma","kota"))
  }
}
