import org.scalatest.FunSuite

class zad01Test extends FunSuite {
  test("zad01") {
    assert(zad01.podziel(List()) == (List(), List()))
    assert(zad01.podziel(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(zad01.podziel(List(0, 0, 0, 0, -1)) == (List(-1), List(-1)))
    assert(zad01.podziel(List(-2, 0, 0, 0)) == (List(-2), List()))
    assert(zad01.podziel(List(1, 2, 3, 4)) == (List(), List()))
    assert(zad01.podziel(List()) == (List(), List()))
  }
}
