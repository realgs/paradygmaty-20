import org.scalatest.FunSuite

class zad04Test extends FunSuite {
  test("zad04.find") {
    assertResult(List("index0168202", "index0168211", "index0168210")) {
      zad04.find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168"))
    }

    assertResult(List("acc", "acc", "bac")) {
      zad04.find(List("abb", "acc", "abb", "acc", "cab", "ca", "bac", "bb", "ab", "bbc"), List("ba", "ac"))
    }

    assertResult(List()) {
      zad04.find(List(), List("aaa", "bbb", "ccc"))
    }

    assertResult(List()) {
      zad04.find(List("abb", "caa", "bca"), List())
    }

    assertResult(List("abb", "caa", "bca")) {
      zad04.find(List("abb", "caa", "bca"), List(""))
    }
  }

  test("zad04.findTail") {
    assertResult(List("index0168202", "index0168211", "index0168210")) {
      zad04.findTail(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168"))
    }

    assertResult(List("acc", "acc", "bac")) {
      zad04.findTail(List("abb", "acc", "abb", "acc", "cab", "ca", "bac", "bb", "ab", "bbc"), List("ba", "ac"))
    }

    assertResult(List()) {
      zad04.findTail(List(), List("aaa", "bbb", "ccc"))
    }

    assertResult(List()) {
      zad04.findTail(List("abb", "caa", "bca"), List())
    }

    assertResult(List("abb", "caa", "bca")) {
      zad04.findTail(List("abb", "caa", "bca"), List(""))
    }
  }
}
