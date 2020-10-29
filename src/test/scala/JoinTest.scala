import org.scalatest.FunSuite

class JoinTest extends FunSuite {

  test("normalLists") {
    assert(L3.join(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }

  test("stringListsTest") {
    assert(L3.join(List("w", "tym"), List("teście", "nie"), List("ma", "gwiazdek")) == List("w", "tym", "teście", "nie", "ma", "gwiazdek"))
  }

  test("emptyLists") {
    assert(L3.join(Nil, Nil, Nil) == Nil)
  }

}
