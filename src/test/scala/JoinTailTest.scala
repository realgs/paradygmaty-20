import org.scalatest.FunSuite

class JoinTailTest extends FunSuite {

  test("normalLists") {
    assert(L3.joinTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }

  test("stringListsTest") {
    assert(L3.joinTail(List("w", "tym"), List("teście", "nie"), List("ma", "gwiazdek")) == List("w", "tym", "teście", "nie", "ma", "gwiazdek"))
  }

  test("emptyLists") {
    assert(L3.joinTail(Nil, Nil, Nil) == Nil)
  }

}
