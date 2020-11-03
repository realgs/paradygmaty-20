import Functions._
import org.scalatest._


class FunctionsTest extends funsuite.AnyFunSuite {
  test("split") {
    assertResult((List(-3, -6, -9), List(-3, -9))) {
      split(List(-3, -6, 8, -9, 13))
    }
    assertResult((List(-2, -2, -2, -2), List())) {
      split(List(-2, -2, -2, -2))
    }
    assertResult((List(-3, -9, -13, -19), List(-3, -9, -13, -19))) {
      split(List(-3, -9, -13, -19))
    }
    assertResult((List(), List())) {
      split(List(1, 2, 3, 4, 5))
    }
    assertResult((List(), List())) {
      split(List())
    }
    assertResult((List(), List())) {
      split(null)
    }
  }

  test("length") {
    assertResult(1000) {
      length(List.range(0, 1000))
    }
    assertResult(4) {
      length(List(5, 4, 3, 2))
    }
    assertResult(0) {
      length(List())
    }
    assertResult(0) {
      length(null)
    }
  }

  test("merge") {
    assertResult(List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)) {
      merge(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))
    }
    assertResult(List(1, 2, 3)) {
      merge(Nil, List(1, 2, 3))
    }
    assertResult(List(1, 2, 3)) {
      merge(List(1, 2, 3), Nil)
    }
    assertResult(List()) {
      merge(Nil, Nil)
    }
  }

  test("find") {
    assertResult(List("index0168210", "index0168211", "index0168202").toSet) {
      find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")).toSet
    }
    assertResult(List("ind01", "xyz02", "ind03", "xyz03").toSet) {
      find(List("ind01", "xyz02", "ind03", "xyz03"), List("ind", "xyz")).toSet
    }
    assertResult(List("ind03", "xyz03").toSet) {
      find(List("ind01", "xyz02", "ind03", "xyz03"), List("03")).toSet
    }
    assertResult(List()) {
      find(List("abc", "test", "cde"), List("xyz"))
    }
    assertResult(List()) {
      find(List(), List("abc", "test", "cde"))
    }
    assertResult(List()) {
      find(List("abc", "test", "cde"), List())
    }
    assertResult(List()) {
      find(List(), List())
    }
  }

  test("findTail") {
    assertResult(List("index0168210", "index0168211", "index0168202").toSet) {
      findTail(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")).toSet
    }
    assertResult(List("ind01", "xyz02", "ind03", "xyz03").toSet) {
      findTail(List("ind01", "xyz02", "ind03", "xyz03"), List("ind", "xyz")).toSet
    }
    assertResult(List("ind03", "xyz03").toSet) {
      findTail(List("ind01", "xyz02", "ind03", "xyz03"), List("03")).toSet
    }
    assertResult(List()) {
      findTail(List("abc", "test", "cde"), List("xyz"))
    }
    assertResult(List()) {
      findTail(List(), List("abc", "test", "cde"))
    }
    assertResult(List()) {
      findTail(List("abc", "test", "cde"), List())
    }
    assertResult(List()) {
      findTail(List(), List())
    }
  }

  test("join") {
    assert(join(List(5, 4, 3, 2), List(1, 0), List(9)) === List(5, 4, 3, 2, 1, 0, 9))
  }

}
