import org.scalatest.FunSuite

class FindTailTest extends FunSuite {

  test("normalInput") {
    assert(L3.findTail(
      words = List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"),
      pattern = "index0168"
    ) == List("index0168202", "index0168211", "index0168210"))
  }

  test("emptyPattern") {
    assert(L3.findTail(List("aa","bb","cc","dd"),"") == List("aa","bb","cc","dd"))
  }

  test("emptyList") {
    assert(L3.findTail(Nil, "aaa") == Nil)
  }

}