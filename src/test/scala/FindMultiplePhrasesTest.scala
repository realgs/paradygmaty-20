import org.scalatest.FunSuite

class FindMultiplePhrasesTest extends FunSuite {

  test("normalInput") {
    assert(L3.findMultiplePhrases(
      words = List("index0169","od tego","index0168211","index0168210","co gada", "nie pasuje"),
      patterns = List("index0168", "tego","gada")
    ) == List("od tego", "index0168211", "index0168210", "co gada"))
  }

  test("emptyPattern") {
    assert(L3.findMultiplePhrases(List("aa","bb","cc","dd"),List("", "")) == List("aa","bb","cc","dd"))
  }

  test("emptyList") {
    assert(L3.findMultiplePhrases(Nil, List("Aa", "Bb", "Cc")) == Nil)
  }

}

