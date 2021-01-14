import org.scalatest.funsuite.AnyFunSuite

class Zad_4Test extends AnyFunSuite {

  val test = new Zad_4

  test("Test not empty list; search() function") {
    assert(test.search(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"),
      "index0168")._1 == List("index0168202", "index0168211", "index0168210"))
  }

  test("Test empty list; search() function") {
    assert(test.search(Nil, "abc")._1 == Nil)
  }

  test("Test null element; search() function") {
    assert(test.search(List("abc", "ala"), null)._1 == Nil)
  }

  test("Test not empty list; searchManyElem() function") {
    assert(test.searchManyElem(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"),
      List("index0168", "index0169222")) == List("index0168202", "index0168211", "index0168210", "index0169222"))
    assert(test.searchManyElem(List("abcd", "bcdefg", "cccc"), List("b")) == List("abcd", "bcdefg"))
    assert(test.searchManyElem(List("abcd", "bcdefg", "cccc"), List("ala", "iza")) == List())
  }

  test("Test empty list; searchManyElem() function") {
    assert(test.searchManyElem(Nil, List("abc", "def")) == Nil)
  }

  test("Test empty element list; searchManyElem() function") {
    assert(test.searchManyElem(List("abc", "def"), Nil) == Nil)
  }

  test("Test not empty list; tailRec_searchManyElem() function") {
    assert(test.tailRec_searchManyElem(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"),
      List("index0168", "index0169222")) == List("index0168202", "index0168211", "index0168210", "index0169222"))
    assert(test.tailRec_searchManyElem(List("abcd", "cccc", "bcdefg"), List("b")) == List("abcd", "bcdefg"))
    assert(test.tailRec_searchManyElem(List("abcd", "bcdefg", "cccc"), List("ala", "iza")) == List())
  }

  test("Test empty list; tailRec_searchManyElem() function") {
    assert(test.tailRec_searchManyElem(Nil, List("abc", "def")) == Nil)
  }

  test("Test empty element list; tailRec_searchManyElem() function") {
    assert(test.tailRec_searchManyElem(List("abc", "def"), Nil) == Nil)
  }

}
