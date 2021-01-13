import org.scalatest.FunSuite

class zad02Test extends FunSuite {
  test("zad02") {
    assertThrows[IllegalArgumentException] {
      zad02.duplicate(List(1, 2, 3, 4, 5, 6, 2), List(2))
    }
    assert(zad02.duplicate(List(1, 2, 3), List(0, 3, 1, 4)) == List(2, 2, 2, 3))
    assert(zad02.duplicate(List(1, 2, 3), List(0, 0, 4, -10)) == List(3, 3, 3, 3))
    assert(zad02.duplicate(List(1, 2, 3), List(1, 2)) == List(1, 2, 2, 3))
    assert(zad02.duplicate(List(1, 2, 3), List(-1, -2, 1)) == List(3))
    assert(zad02.duplicate(List(1, 2, 3), List(-1, -2, -1)) == List())
    assert(zad02.duplicate(List(), List(1, 2)) == List())
    assert(zad02.duplicate(List(1, 2), List()) == List(1, 2))
  }
}
