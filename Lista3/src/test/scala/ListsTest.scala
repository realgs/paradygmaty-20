import org.scalatest.FunSuite

class ListsTest extends FunSuite{
  //listReverse tests

  test("listReverse.empty"){
    assert(Lists.listReverse(Nil) === Nil)
  }

  test("listReverse.full"){
    assert(Lists.listReverse(List(1,2,3,4,5)) === List(5,4,3,2,1))
  }

  //Zad 1 tests
  test("findNegativeAndOddNegative.empty"){
    assert(Lists.findNegativeAndOddNegative(Nil) === (Nil, Nil))
  }

  test("findNegativeAndOddNegative.onlyPositive"){
    assert(Lists.findNegativeAndOddNegative(List(1,2,3,4,5)) === (Nil, Nil))
  }

  test("findNegativeAndOddNegative.onlyNegativeEven"){
    assert(Lists.findNegativeAndOddNegative(List(1,-2,3,-4,5)) === (List(-2,-4), Nil))
  }

  test("findNegativeAndOddNegative.onlyNegativeOdd"){
    assert(Lists.findNegativeAndOddNegative(List(-1,2,-3,4,-5)) === (List(-1,-3,-5), List(-1,-3,-5)))
  }

  test("findNegativeAndOddNegative.allNegative"){
    assert(Lists.findNegativeAndOddNegative(List(-1,-2,-3,-4,-5)) === (List(-1,-2,-3,-4,-5), List(-1,-3,-5)))
  }

  test("findNegativeAndOddNegative.allPossible"){
    assert(Lists.findNegativeAndOddNegative(List(-1,-2,-3,-4,-5,6,10,13)) === (List(-1,-2,-3,-4,-5), List(-1,-3,-5)))
  }

  test("findNegativeAndOddNegative.allPossibleDouble"){
    assert(Lists.findNegativeAndOddNegative(List(-1.0,-2.0,-3.0,-4.0,-5.0,6.0,10.0,13.0)) === (List(-1.0,-2.0,-3.0,-4.0,-5.0), List(-1.0,-3.0,-5.0)))
  }

  //Zad 2 tests
  test("listLenght.empty"){
    assert(Lists.listLenght(Nil) === 0)
  }

  test("listLenght.fullShort"){
    assert(Lists.listLenght(List("Ala", "ma", "kota")) === 3)
  }

  test("listLenght.fullLong"){
    assert(Lists.listLenght(List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) === 26)
  }

  //Zad 3 tests
  test("mergeTwoListsAlternately.empty"){
    assert(Lists.mergeTwoListsAlternately(Nil, Nil) === Nil)
  }

  test("mergeTwoListsAlternately.list1_empty"){
    assert(Lists.mergeTwoListsAlternately(Nil, List(1,2,3)) === List(1,2,3))
  }

  test("mergeTwoListsAlternately.list2_empty"){
    assert(Lists.mergeTwoListsAlternately(List(1,2,3), Nil) === List(1,2,3))
  }

  test("mergeTwoListsAlternately.bothFull"){
    assert(Lists.mergeTwoListsAlternately(List(9,8,7), List(1,2,3)) === List(9,1,8,2,7,3))
  }

  //Zad 5 tests
  test("mergeThreeListsTail.allEmpty"){
    assert(Lists.mergeThreeLists(Nil, Nil, Nil) === Nil)
  }

  test("mergeThreeListsTail.list1&2Empty"){
    assert(Lists.mergeThreeLists(Nil, Nil, List(1,2,3)) === List(1,2,3))
  }

  test("mergeThreeListsTail.list2&3Empty"){
    assert(Lists.mergeThreeLists(List(1,2,3), Nil, Nil) === List(1,2,3))
  }

  test("mergeThreeListsTail.list1&3Empty"){
    assert(Lists.mergeThreeLists(Nil, List(1,2,3), Nil) === List(1,2,3))
  }

  test("mergeThreeListsTail.list1Empty"){
    assert(Lists.mergeThreeLists(Nil, List(0,9,8), List(1,2,3)) === List(0,9,8,1,2,3))
  }

  test("mergeThreeListsTail.list2Empty"){
    assert(Lists.mergeThreeLists(List(0,9,8), Nil, List(1,2,3)) === List(0,9,8,1,2,3))
  }

  test("mergeThreeListsTail.list3Empty"){
    assert(Lists.mergeThreeLists(List(1,2,3), List(0,9,8), Nil) === List(1,2,3,0,9,8))
  }

  test("mergeThreeListsTail.allFull"){
    assert(Lists.mergeThreeLists(List(0,9,8), List(4,5,6), List(1,2,3)) === List(0,9,8,4,5,6,1,2,3))
  }

  test("mergeThreeListsRec.allEmpty"){
    assert(Lists.mergeThreeListsRec(Nil, Nil, Nil) === Nil)
  }

  test("mergeThreeListsRec.list1&2Empty"){
    assert(Lists.mergeThreeListsRec(Nil, Nil, List(1,2,3)) === List(1,2,3))
  }

  test("mergeThreeListsRec.list2&3Empty"){
    assert(Lists.mergeThreeListsRec(List(1,2,3), Nil, Nil) === List(1,2,3))
  }

  test("mergeThreeListsRec.list1&3Empty"){
    assert(Lists.mergeThreeListsRec(Nil, List(1,2,3), Nil) === List(1,2,3))
  }

  test("mergeThreeListsRec.list1Empty"){
    assert(Lists.mergeThreeListsRec(Nil, List(0,9,8), List(1,2,3)) === List(0,9,8,1,2,3))
  }

  test("mergeThreeListsRec.list2Empty"){
    assert(Lists.mergeThreeListsRec(List(0,9,8), Nil, List(1,2,3)) === List(0,9,8,1,2,3))
  }

  test("mergeThreeListsRec.list3Empty"){
    assert(Lists.mergeThreeListsRec(List(1,2,3), List(0,9,8), Nil) === List(1,2,3,0,9,8))
  }

  test("mergeThreeListsRec.allFull"){
    assert(Lists.mergeThreeListsRec(List(0,9,8), List(4,5,6), List(1,2,3)) === List(0,9,8,4,5,6,1,2,3))
  }

  test("mergeThreeListsRec.portal"){
    assert(Lists.mergeThreeListsRec(List("the", "cake"), List("is", "a"), List("lie.")) === List("the", "cake", "is", "a", "lie."))
  }
}
