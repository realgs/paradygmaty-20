import org.scalatest.FunSuite

class zad01Test extends FunSuite {
  test("zad01") {
    assert(zad01.duplicate(List(1, 2, 3), List(0, 3, 1, 4)) == List(2, 2, 2, 3))
    assert(zad01.duplicate(List(1, 2, 3), List(0, 0, 4, -10)) == List(3, 3, 3, 3))
    assert(zad01.duplicate(List(1, 2, 3), List(1, 2)) == List(1, 2, 2, 3))
    assert(zad01.duplicate(List(1, 2, 3), List(-1, -2, 1)) == List(3))
    assert(zad01.duplicate(List(1, 2, 3), List(-1, -2, -1)) == List())
    assert(zad01.duplicate(List(), List(1, 2)) == List())
    assert(zad01.duplicate(List(1, 2), List()) == List(1, 2))
  }
}
