import org.scalatest.FunSuite

class Task4Test extends FunSuite {

  //isPrefix tests
  test("isPrefix.wordEmpty"){
    assert(Task4.isPrefix("", "ocean") === false)
  }

  test("isPrefix.prefixEmpty"){
    assert(Task4.isPrefix("ocean", "") === true)
  }

  test("isPrefix.bothEmpty"){
    assert(Task4.isPrefix("", "") === true)
  }

  test("isPrefix.correct"){
    assert(Task4.isPrefix("ocean", "oc") === true)
  }

  test("isPrefix.correctFull"){
    assert(Task4.isPrefix("ocean", "ocean") === true)
  }

  test("isPrefix.incorrect"){
    assert(Task4.isPrefix("ocean", "c") === false)
  }

  test("isPrefix.longerPrefix"){
    assert(Task4.isPrefix("oc", "ocean") === false)
  }

  //stringContains tests
  test("stringContains.wordEmpty"){
    assert(Task4.stringContains("", "ocean") === false)
  }

  test("stringContains.keyEmpty"){
    assert(Task4.stringContains("ocean", "") === true)
  }

  test("stringContains.bothEmpty"){
    assert(Task4.stringContains("", "") === true)
  }

  test("stringContains.contains"){
    assert(Task4.stringContains("ocean", "ea") === true)
  }

  test("stringContains.containsNot"){
    assert(Task4.stringContains("ocean", "eta") === false)
  }

  test("stringContains.customTest"){
    assert(Task4.stringContains("abaaabbabbbaa", "aab") === true)
  }

  //stringContainsAny tests
  test("stringContainsAny.wordEmpty"){
    assert(Task4.stringContainsAny("", List("ocean", "hill")) === false)
  }

  test("stringContainsAny.keysEmpty"){
    assert(Task4.stringContainsAny("ocean", Nil) === false)
  }

  test("stringContainsAny.bothEmpty"){
    assert(Task4.stringContainsAny("", Nil) === false)
  }

  test("stringContainsAny.containsFirst"){
    assert(Task4.stringContainsAny("ocean", List("ocean", "hill")) === true)
  }

  test("stringContainsAny.containsLast"){
    assert(Task4.stringContainsAny("ocean", List("hill", "ocean")) === true)
  }

  test("stringContainsAny.containsOne"){
    assert(Task4.stringContainsAny("ocean", List("ocean")) === true)
  }

  test("stringContainsAny.containsEmptyKey"){
    assert(Task4.stringContainsAny("ocean", List("hill", "")) === true)
  }

  test("stringContainsAny.custom"){
    assert(Task4.stringContainsAny("abaaabbabbbaa", List("hill", "aab")) === true)
  }

  //Task 4 main function (one pattern, recursive) tests
  test("findMatchOneRec.listEmpty"){
    assert(Task4.findMatchOneRec(Nil, "oc") === Nil)
  }

  test("findMatchOneRec.bothEmpty"){
    assert(Task4.findMatchOneRec(Nil, "") === Nil)
  }

  test("findMatchOneRec.patternEmpty"){
    assert(Task4.findMatchOneRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "") === List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchOneRec.resultThree"){
    assert(Task4.findMatchOneRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") === List("index0168202","index0168211","index0168210"))
  }

  test("findMatchOneRec.patternWrong"){
    assert(Task4.findMatchOneRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "indeks") === Nil)
  }

  test("findMatchOneRec.oneExactMatch"){
    assert(Task4.findMatchOneRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0169224") === List("index0169224"))
  }

  //Task 4 main function (one pattern, tail recursive) tests
  test("findMatchOne.listEmpty"){
    assert(Task4.findMatchOne(Nil, "oc") === Nil)
  }

  test("findMatchOne.bothEmpty"){
    assert(Task4.findMatchOne(Nil, "") === Nil)
  }

  test("findMatchOne.patternEmpty"){
    assert(Task4.findMatchOne(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "") === List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchOne.resultThree"){
    assert(Task4.findMatchOne(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") === List("index0168202","index0168211","index0168210"))
  }

  test("findMatchOne.patternWrong"){
    assert(Task4.findMatchOne(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "indeks") === Nil)
  }

  test("findMatchOne.oneExactMatch"){
    assert(Task4.findMatchOne(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0169224") === List("index0169224"))
  }

  //Task 4 main function (n patterns, recursive) tests
  test("findMatchManyRec.listEmpty"){
    assert(Task4.findMatchManyRec(Nil, List("ocean", "hill")) === Nil)
  }

  test("findMatchManyRec.patternsEmpty"){
    assert(Task4.findMatchManyRec(List("ocean", "hill"), Nil) === Nil)
  }

  test("findMatchManyRec.bothEmpty"){
    assert(Task4.findMatchManyRec(Nil, Nil) === Nil)
  }
  test("findMatchManyRec.onePattern"){
    assert(Task4.findMatchManyRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168")) === List("index0168202","index0168211","index0168210"))
  }

  test("findMatchManyRec.twoPatterns"){
    assert(Task4.findMatchManyRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index01692")) === List("index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchManyRec.allToOne"){
    assert(Task4.findMatchManyRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("ocean", "index")) === List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchManyRec.manyExactMatches"){
    assert(Task4.findMatchManyRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168211", "index0169222", "index0169224", "index0168210")) === List("index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchManyRec.manyPatterns"){
    assert(Task4.findMatchManyRec(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168211", "index0169222", "index016922", "index016821")) === List("index0168211","index0168210","index0169222","index0169224"))
  }

  //Task 4 main function (n patterns, tail recursive) tests
  test("findMatchMany.listEmpty"){
    assert(Task4.findMatchMany(Nil, List("ocean", "hill")) === Nil)
  }

  test("findMatchMany.patternsEmpty"){
    assert(Task4.findMatchMany(List("ocean", "hill"), Nil) === Nil)
  }

  test("findMatchMany.bothEmpty"){
    assert(Task4.findMatchMany(Nil, Nil) === Nil)
  }
  test("findMatchMany.onePattern"){
    assert(Task4.findMatchMany(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168")) === List("index0168202","index0168211","index0168210"))
  }

  test("findMatchMany.twoPatterns"){
    assert(Task4.findMatchMany(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index01692")) === List("index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchMany.allToOne"){
    assert(Task4.findMatchMany(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("ocean", "index")) === List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchMany.manyExactMatches"){
    assert(Task4.findMatchMany(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168211", "index0169222", "index0169224", "index0168210")) === List("index0168211","index0168210","index0169222","index0169224"))
  }

  test("findMatchMany.manyPatterns"){
    assert(Task4.findMatchMany(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168211", "index0169222", "index016922", "index016821")) === List("index0168211","index0168210","index0169222","index0169224"))
  }
}
