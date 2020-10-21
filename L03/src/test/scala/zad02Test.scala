import org.scalatest.FunSuite

class zad02Test extends FunSuite {
  test("zad02") {
    assert(zad02.dlugosc(List(5, 4, 3, 2)) == 4)
    assert(zad02.dlugosc(List()) == 0)
    assert(zad02.dlugosc(List(1, 2, 3)) == 3)
    assert(zad02.dlugosc(List(0, 0, 0, 0, 0)) == 5)
  }
}
