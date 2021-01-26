import org.scalatest.FunSuite

class zad04Test extends FunSuite {
  test("zad04") {
    assert(zad04.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    assert(zad04.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3))
    assert(zad04.eachNElement(LazyList(), 2, 4) == LazyList())
    assert(zad04.eachNElement(LazyList(1, 2, 3, 4), 1, 4) == LazyList(1, 2, 3, 4))
    assert(zad04.eachNElement(LazyList(1, 2, 3, 4), 1, 2) == LazyList(1, 2))
    assert(zad04.eachNElement(LazyList(1, 2, 3, 4), 4, 2) == LazyList(1))
    assert(zad04.eachNElement(LazyList(1, 2, 3, 4), 4, -1) == LazyList())
    assertThrows[IllegalArgumentException]{zad04.eachNElement(LazyList(1, 2, 3, 4), -2, 2)}
  }
}
