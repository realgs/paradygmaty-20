import org.scalatest.FunSuite

class DivideListTest extends FunSuite {

  test("normalList") {
    assert(L3.divideList(List(-1, -2, -3, -4, 5, 6, -7, -8)) == (List(-1,-2,-3,-4,-7,-8), List(-1,-3,-7)))
  }

  test("positiveList") {
    assert(L3.divideList(List(1, 2, 3, 4, 5, 6, 7, 8)) == (Nil, Nil))
  }

  test("negativeEvenList") {
    assert(L3.divideList(List(-2, -4, -6, -8)) == (List(-2, -4, -6, -8), Nil))
  }

  test("sameElementsList") {
    assert(L3.divideList(List(-5,-5,-5,-5,-5)) == (List(-5,-5,-5,-5,-5),List(-5,-5,-5,-5,-5)))
  }

  test("emptyList") {
    assert(L3.divideList(Nil) == (Nil, Nil))
  }

}
