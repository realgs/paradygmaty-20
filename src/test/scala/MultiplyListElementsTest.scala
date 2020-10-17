import org.scalatest.FunSuite

class MultiplyListElementsTest extends FunSuite {

  test("positiveIntegersList") {
    assert(L2.multiplyListElements(List(1, 2, 3, 4, 5)) == 120)
  }

  test("negativeIntegersList") {
    assert(L2.multiplyListElements(List(-10, -5, -2)) == -100)
  }

  test("mixedDoublesList") {
    assert(L2.multiplyListElements(List(-2.5, 2.40, -3.1, -3.0, 4.2)) == -234.36)
  }

  test("emptyList") {
    assert(L2.multiplyListElements(List()) == 0)
  }

}
