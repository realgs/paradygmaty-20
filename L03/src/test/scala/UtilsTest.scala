import org.scalatest.FunSuite

class UtilsTest extends FunSuite {
  test("reverseList") {
    assert(Utils.reverseList(List(1, 2, 3)) == List(3, 2, 1))
    assert(Utils.reverseList(List("qwerty")) == List("qwerty"))
    assert(Utils.reverseList(List()) == List())
  }

  test("appendList") {
    assert(Utils.appendList(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    assert(Utils.appendList(List(), List(3, 4)) == List(3, 4))
    assert(Utils.appendList(List(1, 2), List()) == List(1, 2))
    assert(Utils.appendList(List(), List()) == List())
  }
}
