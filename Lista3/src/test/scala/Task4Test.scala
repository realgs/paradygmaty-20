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

  //Task 4 main function (recursive) tests
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

  //Task 4 main function (tail recursive) tests
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
}
