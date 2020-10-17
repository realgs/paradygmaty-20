import org.scalatest.FunSuite

class AreAllElementsInRangeTest extends FunSuite{

  test("allElementsInRange") {
    assert(L2.areAllElementsInRange(List(5.5, 12.7, 8.9, 10), 4.5, 14.9))
  }

  test("notAllElementsInRange") {
    assert(!L2.areAllElementsInRange(List(8.9, 12.7, 5.5, 10), 7.2, 14.9))
  }

  test("extremeValuesList") {
    assert(L2.areAllElementsInRange(List(19.8, 14.2), 14.2, 19.8))
  }

  test("emptyList") {
    assert(L2.areAllElementsInRange(Nil, 0, 1))
  }

  test("sameRangeValues") {
    assert(L2.areAllElementsInRange(List(2.8, 2.8, 2.8), 2.8, 2.8))
  }

  test("wrongRangeInput") {
    assertThrows[Exception] { L2.areAllElementsInRange(List(1,2,3), 18, 9) }
  }
}
