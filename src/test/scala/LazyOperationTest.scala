import org.scalatest.FunSuite

class LazyOperationTest extends FunSuite{

  test("basicTest") {
    assert(Lista4.lazyOperation(LazyList(1,2,3),LazyList(2,3,4,5), '+') == LazyList(3,5,7,5))
  }

  test("basicTest2") {
    assert(Lista4.lazyOperation(LazyList(6,3,4,5),LazyList(2), '/') == LazyList(3,3,4,5))
  }

  test("twoEmptyLists") {
    assert(Lista4.lazyOperation(LazyList(), LazyList(), '+') == LazyList())
  }

  test("oneEmptyList") {
    assert(Lista4.lazyOperation(LazyList(), LazyList(1,2,3), '-') == LazyList(1,2,3))
  }

  test("unsupportedOperation") {
    assertThrows[IllegalArgumentException](Lista4.lazyOperation(LazyList(1,2,3), LazyList(4,5,6), '%'))
  }

}
