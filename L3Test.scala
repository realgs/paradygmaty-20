import org.scalatest.FunSuite
import List3.L3

class L3Test extends FunSuite {

  test("Test for 'contains' function:") {
    assert(!L3.contains("Ala", "ma"))
    assert(L3.contains("rabarbar", "bar"))
    assert(!L3.contains("test1063", "test0"))
    assert(L3.contains("aab", "ab"))
    assert(L3.contains("aaad", "ad"))
    assert(L3.contains("aaad", "aad"))
  }

  test("Test for 'negativeLists' function:") {
    assert(L3.negativeLists(List(1, -11, -9, -4, -11, -4, -2, 11, 0, 25)) == (List(-11, -9, -4, -11, -4, -2), List(-11, -9, -11)))
    assert(L3.negativeLists(List()) == (List(), List()))
    assert(L3.negativeLists(List(-2, 4, -8, -10, 0)) == (List(-2, -8, -10),List()))
    assert(L3.negativeLists(List(0, 8, 7, 10)) == (List(), List()))
    assert(L3.negativeLists(List(-4.5, -9, -3.1, -2)) == (List(-4.5, -9,-3.1,-2), List(-9)))
  }

  test("Test for 'countLength' function:") {
    assert(L3.countLength(List()) == 0)
    assert(L3.countLength(List(1, 2, 3, -9)) == 4)
    assert(L3.countLength(List("Ala", "ma", "kota")) == 3)
    assert(L3.countLength(List(List(),List(), List(8,0))) == 3)
  }

  test("Test for 'mergeLists' function:") {
    assert(L3.mergeLists(List(1,3,5,7,9), List(2,4,6,8,10)) == List(1,2,3,4,5,6,7,8,9,10))
    assert(L3.mergeLists(List(),List("Ala","ma","kota")) == List("Ala","ma","kota"))
    assert(L3.mergeLists(List(2.4,2.7,90,0), List()) == List(2.4,2.7,90,0))
    assert(L3.mergeLists(List(),List()) == List())
    assert(L3.mergeLists(List("a","b","c"), List("d","e","f","g","h")) == List("a","d","b","e","c","f","g","h"))
  }

  test("Test for 'findElem' function:") {
    assert(L3.findElem(List("23456", "768", "4325", "234", "234579", "1234", "334235", "12343"), "234") == List("23456", "234", "234579", "1234", "12343"))
    assert(L3.findElem(List("rabarbar", "bar", "barbara", "barka", "robak", "kabaret"), "bar") == List("rabarbar", "bar", "barbara", "barka", "kabaret"))
    assert(L3.findElem(List("Ala", "ma", "kota"), "pies") == List())
    assert(L3.findElem(List(), "error") == List())
    assert(L3.findElem(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") == List("index0168202","index0168211", "index0168210"))
  }

  test("Test for 'findElemTail' function:") {
    assert(L3.findElemTail(List("23456", "768", "4325", "234", "234579", "1234", "334235", "12343"), "234") == List("23456", "234", "234579", "1234", "12343"))
    assert(L3.findElemTail(List("rabarbar", "bar", "barbara", "barka", "robak", "kabaret"), "bar") == List("rabarbar", "bar", "barbara", "barka", "kabaret"))
    assert(L3.findElemTail(List("Ala", "ma", "kota"), "pies") == List())
    assert(L3.findElemTail(List(), "error") == List())
    assert(L3.findElemTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") == List("index0168202","index0168211", "index0168210"))
  }

  test("Test for 'findMoreElem' function:") {
    assert(L3.findMoreElem(List("aaad", "aad", "ma", "ada"), List("ad", "sto")) == List("aaad", "aad", "ada"))
    assert(L3.findMoreElem(List("1234", "23456", "34567", "90", "09876", "678", "5678"), List("345", "09", "1")) == List("1234", "23456", "34567", "09876"))
    assert(L3.findMoreElem(List(), List("ad", "3525", "53fsdg")) == List())
    assert(L3.findMoreElem(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List()) == List())
    assert(L3.findMoreElem(List("index0169","index168202","index0168211","index168210","index169222","index169224"), List("index6", "82", "x0")) == List("index0169", "index168202","index0168211","index168210"))
  }

  test("Test for 'findMoreElemTail' function:") {
    assert(L3.findMoreElemTail(List("aaad", "aad", "ma", "ada"), List("ad", "sto")) == List("aaad", "aad", "ada"))
    assert(L3.findMoreElemTail(List("1234", "23456", "34567", "90", "09876", "678", "5678"), List("345", "09", "1")) == List("1234", "23456", "34567", "09876"))
    assert(L3.findMoreElemTail(List(), List("ad", "3525", "53fsdg")) == List())
    assert(L3.findMoreElemTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List()) == List())
    assert(L3.findMoreElemTail(List("index0169","index168202","index0168211","index168210","index169222","index169224"), List("index6", "82", "x0")) == List("index0169", "index168202","index0168211","index168210"))
  }

  test("Test for 'joinLists' function:") {
    assert(L3.joinLists(List(1,2,3), List(4,5), List(6,7)) == List(1,2,3,4,5,6,7))
    assert(L3.joinLists(List("Ala","ma"), List(), List("kota")) == List("Ala", "ma", "kota"))
    assert(L3.joinLists(List(), List(4,5), List()) == List(4,5))
    assert(L3.joinLists(List(), List(), List()) == List())
    assert(L3.joinLists(List(), List(), List(3.5,1.9,-9.2)) == List(3.5,1.9,-9.2))
  }

  test("Test for 'joinListsTail' function:") {
    assert(L3.joinListsTail(List(1,2,3), List(4,5), List(6,7)) == List(1,2,3,4,5,6,7))
    assert(L3.joinListsTail(List("Ala","ma"), List(), List("kota")) == List("Ala", "ma", "kota"))
    assert(L3.joinListsTail(List(), List(4,5), List()) == List(4,5))
    assert(L3.joinListsTail(List(), List(), List()) == List())
    assert(L3.joinListsTail(List(), List(), List(3.5,1.9,-9.2)) == List(3.5,1.9,-9.2))
  }
}