import org.scalatest.FunSuite

class zad03Test extends FunSuite {
  test("zad03") {
    assert(zad03.polacz(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
    assert(zad03.polacz(List(), List(1, 2, 3, 4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(zad03.polacz(List(5, 4, 3, 2), List()) == List(5, 4, 3, 2))
    assert(zad03.polacz(List(), List()) == List())
    assert(zad03.polacz(List("elo", "3", "0"), List("elo", "2")) == List("elo", "elo", "3", "2", "0"))
  }
}
