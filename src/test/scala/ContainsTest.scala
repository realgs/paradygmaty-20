import org.scalatest.FunSuite

class ContainsTest extends FunSuite {

  test("contain") {
    assert(L3.contains("index0168211", "index0168"))
  }

  test("notContain") {
    assert(!L3.contains("abdef", "zw"))
  }

  test("emptyWord") {
    assert(!L3.contains("", "zw"))
  }

  test("emptyPattern") {
    assert(L3.contains("zw", ""))
  }

}
