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

  test("length.emptyList") {
    assert(Functions.length(Nil) === 0)
  }

  test("length.longList") {
    assert(Functions.length(List.fill(50000)(1)) === 50000)
  }

  // Task 3, not tested properly
  test("interlace.example") {
    assert(Functions.interlace(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) === List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
  }

  test("interlace.differentLengths") {
    assert(Functions.interlace(List(1, 2, 3, 4, 5), List(6, 7, 8)) === List(1, 6, 2, 7, 3, 8, 4, 5))
  }

  test("interlace.emptyLists") {
    assert(Functions.interlace(Nil, Nil) === Nil)
  }

  test("interlace.firstEmpty") {
    assert(Functions.interlace(List(1, 2, 3, 4), Nil) === List(1, 2, 3, 4))
  }

  test("interlace.secondEmpty") {
    assert(Functions.interlace(Nil, List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
  }

  test("isSubstring.beginningOccurrence") {
    assert(Functions.isSubstring("beta", "beta_alpha_gamma"))
  }

  test("isSubstring.endOccurrence") {
    assert(Functions.isSubstring("mma", "beta_alpha_gamma"))
  }

  test("isSubstring.middleOccurrence") {
    assert(Functions.isSubstring("lpha", "beta_alpha_gamma"))
  }

  test("isSubstring.noOccurrence") {
    assert(!Functions.isSubstring("kar", "beta_alpha_gamma"))
  }

  test("isSubstring.identicalString") {
    assert(Functions.isSubstring("beta_alpha_gamma", "beta_alpha_gamma"))
  }

  test("find.example") {
    assert(Functions.find("index0168",
      List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")) ===
      List("index0168202", "index0168211", "index0168210"))
  }

  test("find.multipleExample") {
    assert(Functions.find(List("682", "0168202"),
      List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")) ===
      List("index0168202", "index0168211", "index0168210"))
  }

  test("joinLists.example") {
    assert(Functions.joinLists(List(5,4,3,2), List(1,0), List(9)) === List(5,4,3,2,1,0,9))
  }

  test("joinLists.simple") {
    assert(Functions.joinLists(List(1, 2, 3, 4, 5), List(6, 7, 8, 9), List(10, 11)) ===
      List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  }
}
