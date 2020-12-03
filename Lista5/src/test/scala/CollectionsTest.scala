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
  test("duplicate.duplicateElements") {
    assert(Collections.duplicate(Queue(1, 1, 3), Queue(3, 2, 1)) == Queue(1,1,1,1,1,3))
  }
  test("duplicate.equalSizes_String") {
    assert(Collections.duplicate(Queue("Ala", "ma", "kota"), Queue(3, 2, 1)) == Queue("Ala","Ala","Ala","ma","ma","kota"))
  }

  //Zad 2 tests
  test("duplicateNoEntryDuplicates.bothEmpty") {
    assert(Collections.duplicateNoEntryDuplicates(Set(), Queue()) == Queue())
  }
  test("duplicateNoEntryDuplicates.repetitionsEmpty") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 2, 3), Queue()) == Queue())
  }
  test("duplicateNoEntryDuplicates.elementsEmpty") {
    assert(Collections.duplicateNoEntryDuplicates(Set(), Queue(1, 2, 3)) == Queue())
  }
  test("duplicateNoEntryDuplicates.equalSizes") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 2, 3), Queue(3, 2, 1)) == Queue(1,1,1,2,2,3))
  }
  test("duplicateNoEntryDuplicates.lessRepetitions") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 2, 3), Queue(3, 2)) == Queue(1,1,1,2,2))
  }
  test("duplicateNoEntryDuplicates.moreRepetitions") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 2, 3), Queue(3, 2, 1, 4)) == Queue(1,1,1,2,2,3))
  }
  test("duplicateNoEntryDuplicates.negativeRepetitions") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 2, 3), Queue(3, -2, 1)) == Queue(1,1,1,3))
  }
  test("duplicateNoEntryDuplicates.duplicateElements") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 1, 3), Queue(3, 2, 1)) == Queue(1,1,1,3,3))
  }
  test("duplicateNoEntryDuplicates.duplicateElements_2") {
    assert(Collections.duplicateNoEntryDuplicates(Set(1, 3, 1, 3, 1), Queue(3, 2, 1, 2, 3)) == Queue(1,1,1,3,3))
  }
  test("duplicateNoEntryDuplicates.equalSizes_String") {
    assert(Collections.duplicateNoEntryDuplicates(Set("Ala", "ma", "kota"), Queue(3, 2, 1)) == Queue("Ala","Ala","Ala","ma","ma","kota"))
  }
}
