import org.scalatest.funsuite.AnyFunSuite

class LazyLists_Test extends AnyFunSuite {

  val LL = new LazyLists

  //Zadanie 4
  test("Test for ints; eachNElement function") {
    val lxs = LazyList(5, 6, 3, 2, 1)
    assert(LL.eachNElement(lxs, 2, 3) == LazyList(5, 3))
    assert(LL.eachNElement(lxs, 2, 4) == LazyList(5, 3))
    assert(LL.eachNElement(lxs, 2, 5) == LazyList(5, 3, 1))
  }

  test("Test for strings; eachNElement function") {
    val lxs = LazyList("a", "b", "c", "d")
    assert(LL.eachNElement(lxs, 3, 7) == LazyList("a", "d"))
    assert(LL.eachNElement(lxs, 1, 4) == LazyList("a", "b", "c", "d"))
  }

  test("Test empty list; eachNElement function") {
    assert(LL.eachNElement(LazyList(), 2, 3) == LazyList())
  }

  //Zadanie 5
  test("Test empty list; doOperation function") {
    assert(LL.doOperation(LazyList(), LazyList(1, 2, 3), "add") == LazyList())
    assert(LL.doOperation(LazyList(1, 2, 3), LazyList(), "add") == LazyList())
  }

  test("Test operations; doOperation function") {
    val la = LazyList(10, 8, 6)
    val lb = LazyList(2, 4, 6, 2, 2, 2)
    assert(LL.doOperation(la, lb, "add") == LazyList(12, 12, 12))
    assert(LL.doOperation(la, lb, "subtract") == LazyList(8, 4, 0))
    assert(LL.doOperation(la, lb, "multiply") == LazyList(20, 32, 36))
    assert(LL.doOperation(la, lb, "divide") == LazyList(5, 2, 1))
  }

  test("Test empty list; doOperationF function") {
    assert(LL.doOperationF(LazyList(), LazyList(1.4, 2.5, 3.6), "add") == LazyList())
    assert(LL.doOperationF(LazyList(1.4, 2.5, 3.6), LazyList(), "add") == LazyList())
  }

  test("Test operations; doOperationF function") {
    val la = LazyList(10.6, 8.4, 20.2)
    val lb = LazyList(2.0, 4.0, 2.0, 2.0, 2.0, 2.0)
    assert(LL.doOperationF(la, lb, "add") == LazyList(12.6, 12.4, 22.2))
    assert(LL.doOperationF(la, lb, "subtract") == LazyList(8.6, 4.4, 18.2))
    assert(LL.doOperationF(la, lb, "multiply") == LazyList(21.2, 33.6, 40.4))
    assert(LL.doOperationF(la, lb, "divide") == LazyList(5.3, 2.1, 10.1))
  }

  test("Test exception; doOperation function") {
    assertThrows[IllegalArgumentException] {
      assert(LL.doOperation(LazyList(), LazyList(1, 2, 3), "something") == LazyList())
    }
  }

}
