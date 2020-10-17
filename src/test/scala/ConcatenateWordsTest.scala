import org.scalatest.FunSuite

class ConcatenateWordsTest extends FunSuite {

  test("normalWords") {
    assert(L2.concatenateWords(List("Testuje", "fajne", "zadanie"), ' ', '!') == "Testuje fajne zadanie!")
  }

  test("allSameArguments") {
    assert(L2.concatenateWords(List("!","!"), '!', '!') == "!!!!")
  }

  test("emptyWordsList") {
    assert(L2.concatenateWords(Nil, ' ', '!') == "!")
  }
}
