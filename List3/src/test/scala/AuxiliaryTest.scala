import org.scalatest.FunSuite

import Auxiliary._

class AuxiliaryTest extends FunSuite {
  test("fold_left.addition") {
    assert(fold_left(List(1, 2, 3, 4), 0)(_ + _) === 10)
  }

  test("reverse.simpleList") {
    assert(reverse(List(1, 2, 3, 4, 5)) === List(5, 4, 3, 2, 1))
  }

  test("reverse.emptyList") {
    assert(reverse(List()) === List())
  }

  test("reverse.homogeneousLong") {
    assert(reverse(List.fill(1)(200000)) === List.fill(1)(200000))
  }
}
