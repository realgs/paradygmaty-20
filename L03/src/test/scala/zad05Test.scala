import org.scalatest.FunSuite

class zad05Test extends FunSuite {
  test("zad05.joinLists") {
    assert(zad05.joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(zad05.joinLists(List(), List(), List()) == List())
    assert(zad05.joinLists(List(), List(), List(1, 2, 3)) == List(1, 2, 3))
    assert(zad05.joinLists(List(1, 2, 3, 4), List(), List(3, 2, 1)) == List(1, 2, 3, 4, 3, 2, 1))
  }

  test("zad05.joinListsTail") {
    assert(zad05.joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(zad05.joinListsTail(List(), List(), List()) == List())
    assert(zad05.joinListsTail(List(), List(), List(1, 2, 3)) == List(1, 2, 3))
    assert(zad05.joinListsTail(List(1, 2, 3, 4), List(), List(3, 2, 1)) == List(1, 2, 3, 4, 3, 2, 1))
  }
}
