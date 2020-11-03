import org.scalatest._
import matchers.should._

class FunctionsTest extends funsuite.AnyFunSuite with Matchers {
  test("split") {
    assert(Functions.split(List(-3,-6,8,-9,13)) === (List(-3,-6,-9), List(-3,-9)))
    assert(Functions.split(List(-2, -2, -2, -2)) === (List(-2, -2, -2, -2), List()))
    assert(Functions.split(List(-3, -9, -13, -19)) === (List(-3, -9, -13, -19), List(-3, -9, -13, -19)))
    assert(Functions.split(List(1, 2, 3, 4, 5)) === (List(), List()))
    assert(Functions.split(List()) === (List(), List()))
    assert(Functions.split(null) === (List(), List()))
  }

  test("length") {
    assert(Functions.length(List.range(0, 1000)) === 1000)
    assert(Functions.length(List(5,4,3,2)) === 4)
    assert(Functions.length(List()) === 0)
    assert(Functions.length(null) === 0)
  }

  test("merge") {
    assert(Functions.merge(List(5,4,3,2), List(1,2,3,4,5,6)) === List(5,1,4,2,3,3,2,4,5,6))
    assert(Functions.merge(Nil, List(1, 2, 3)) === List(1, 2, 3))
    assert(Functions.merge(List(1, 2, 3), Nil) === List(1, 2, 3))
    assert(Functions.merge(Nil, Nil) === List())
  }

  test("find") {
    assert(Functions.find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168")) ===
      List("index0168210", "index0168211", "index0168202"))
    assert(Functions.find(List("abc", "test", "cde"), List("xyz")) === List())
    assert(Functions.find(List(), List("test")) === List())
    assert(Functions.find(List("ind01", "ind02", "ind03", "ind03"), List("ind")) === List("ind03", "ind03", "ind02", "ind01"))


  }

  test("join") {
    assert(Functions.join(List(5,4,3,2), List(1,0), List(9)) === List(5,4,3,2,1,0,9))
  }

}
