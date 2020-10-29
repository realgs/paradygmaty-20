import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("fold_left.addition") {
    assert(Functions.fold_left(List(1, 2, 3, 4), 0)(_ + _) === 10)
  }

  test("reverse.simpleList") {
    assert(Functions.reverse(List(1, 2, 3, 4, 5)) === List(5, 4, 3, 2, 1))
  }

  test("reverse.emptyList") {
    assert(Functions.reverse(List()) === List())
  }

  test("reverse.homogeneousLong") {
    assert(Functions.reverse(List.fill(1)(200000)) === List.fill(1)(200000))
  }

  test("split.example") {
    assert(Functions.split(List(-3, -6, 8, -9, 13)) === (List(-3, -6, -9), List(-3, -9)))
  }
}
