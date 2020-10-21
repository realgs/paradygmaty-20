import org.scalatest.FunSuite

class UtilsTest extends FunSuite {
  test("reverseList") {
    assert(Utils.reverseList(List(1, 2, 3)) == List(3, 2, 1))
    assert(Utils.reverseList(List("qwerty")) == List("qwerty"))
    assert(Utils.reverseList(List()) == List())
  }
}
