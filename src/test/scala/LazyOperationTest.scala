import org.scalatest.FunSuite

// testy do zadania 5 (5 punktow)
class LazyOperationTest extends FunSuite {

  test("addition") {
    assert(Lista4.lazyOperation(LazyList(1, 2, 3), LazyList(2, 3, 4, 5))((a, b) => a + b) == LazyList(3, 5, 7, 5))
  }

  test("subtraction") {
    assert(Lista4.lazyOperation(LazyList(4.8, 12.3, 5.0), LazyList(1.8, 2.3, 5.0, 49.5))((a, b) => a - b) == LazyList(3.0, 10.0, 0.0, 49.5))
  }

  test("multiplication") {
    assert(Lista4.lazyOperation(LazyList(4, 8), LazyList(2, 3, 4, 5))((a, b) => a * b) == LazyList(8, 24, 4, 5))
  }

  test("division") {
    assert(Lista4.lazyOperation(LazyList(6.8, 3.2, 4.4, 5.0), LazyList(2.0, 2.0))((number1, number2) => number1 / number2) == LazyList(3.4, 1.6, 4.4, 5.0))
  }

  test("twoEmptyLists") {
    assert(Lista4.lazyOperation(LazyList(), LazyList())(null) == LazyList())
  }

  test("oneEmptyList") {
    assert(Lista4.lazyOperation(LazyList(), LazyList(1, 2, 3))((number1, number2) => number1 - number2) == LazyList(1, 2, 3))
  }
}
