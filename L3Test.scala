package List3Test
import org.scalatest.FunSuite
import L3.L3

class L3Test extends FunSuite {
  test("Test of helper function 'reverse'") {
    assert(L3.reverse(Nil) == Nil)
    assert(L3.reverse(List('a','b','c','d')) == List('d','c','b','a'))
    assert(L3.reverse(List(1.5,2.5)) == List(2.5,1.5))
    assert(L3.reverse(List(1,2,3,4,5,6,7,8,9)) == List(9,8,7,6,5,4,3,2,1))
  }

  test("Test of helper function 'contains'") {
    assert(L3.contains ("aaab", "aab"))
    assert(L3.contains("osa", "s"))
    assert(!L3.contains("test1065", "test0"))
    assert(L3.contains("aaaad", "aaad"))
  }

  test("Test of task 1: 'divide' function") {
    assert(L3.divide(List(1,-2,-4,-5,-7)) == (List(-2,-4,-5, -7), List(-5,-7)))
    assert(L3.divide(List(1,-2,-2,-5,-5)) == (List(-2,-2,-5, -5), List(-5,-5)))
    assert(L3.divide(List(0,0,7)) == (Nil, Nil))
    assert(L3.divide(Nil) == (Nil, Nil))
    assert(L3.divide(List(-1,-2,-3,-4,-5,-6,7,8,-9)) == (List(-1,-2,-3,-4,-5,-6,-9), List(-1,-3,-5,-9)))
    assert(L3.divide(List(-1.5,-2,-3,-7.86, -81)) == (List(-1.5,-2,-3,-7.86, -81), List(-3, -81)))
  }

  test("Test of task 2: 'listLength' function") {
    assert(L3.listLength(List(1,-2,-4,-5,-7)) == 5)
    assert(L3.listLength(List(0,0,7)) == 3)
    assert(L3.listLength(Nil) == 0)
    assert(L3.listLength(List("Have", "a", "nice", "day")) == 4)
  }

  test("Test of task 3: 'mergeLists' function") {
    assert(L3.mergeLists(List(1,3,5), List(2,4,6)) == List(1,2,3,4,5,6))
    assert(L3.mergeLists(List("Have", "a"), List("nice", "day")) == List("Have", "nice", "a", "day"))
    assert(L3.mergeLists(List(1.0,2.0,3.0,4.0,5.0), List(1.5,2.5,3.5,4.5)) == List(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0))
    assert(L3.mergeLists(Nil, List(1,2)) == List(1,2))
    assert(L3.mergeLists(List(11.5,15.5), Nil) == List(11.5,15.5))
    assert(L3.mergeLists(Nil, Nil) == Nil)
  }

  test("Test of task 4: 'find' function (for finding only one pattern)") {
    assert(L3.find(List("test0123", "test1065", "test0test", "test", "test077"), "test0") == List("test0123", "test0test", "test077"))
    assert(L3.find(List("11", "211", "12"), "1") == List("11", "211", "12"))
    assert(L3.find(Nil, "2") == Nil)
    assert(L3.find(List("test", "of", "find", "function"), "") == List("test", "of", "find", "function"))
    assert(L3.find(List("test", "of", "find", "function"), "o") == List("of", "function"))
  }

  test("Test of task 4: 'findTailRec' function (for finding only one pattern)") {
    assert(L3.findTailRec(List("test0123", "test1065", "test0test", "test", "test077"), "test0") == List("test0123", "test0test", "test077"))
    assert(L3.findTailRec(List("11", "211", "12"), "1") == List("11", "211", "12"))
    assert(L3.findTailRec(Nil, "2") == Nil)
    assert(L3.findTailRec(List("test", "of", "find", "function"), "") == List("test", "of", "find", "function"))
    assert(L3.findTailRec(List("test", "of", "find", "function"), "o") == List("of", "function"))
  }

  test("Test of task 4: 'findMany' function (for finding N patterns") {
    assert(L3.findMany(List("test0123", "test1065", "test0test", "test", "test077"), List("test0", "7")) == List("test0123", "test0test", "test077"))
    assert(L3.findMany(List("11", "21", "12", "45", "648"), List("1", "4")) == List("11", "21", "12", "45", "648"))
    assert(L3.findMany(Nil, List("2")) == Nil)
    assert(L3.findMany(List("test", "of", "find", "function"), List("")) == List("test", "of", "find", "function"))
    assert(L3.findMany(List("test", "of", "find", "function"), List("o", "f")) == List("of", "find", "function"))
    assert(L3.findMany(List("book", "256", "okay", "65", "76", "empty", "level", "frame", "elevator"), List("ok", "5", "ev")) == List("book", "256", "okay","65", "level", "elevator"))
  }

  test("Test of task 4: 'findManyTailRec' function (for finding N patterns") {
    assert(L3.findManyTailRec(List("test0123", "test1065", "test0test", "test", "test077"), List("test0", "7")) == List("test0123", "test0test", "test077"))
    assert(L3.findManyTailRec(List("11", "21", "12", "45", "648"), List("1", "4")) == List("11", "21", "12", "45", "648"))
    assert(L3.findManyTailRec(Nil, List("2")) == Nil)
    assert(L3.findManyTailRec(List("test", "of", "find", "function"), List("")) == List("test", "of", "find", "function"))
    assert(L3.findManyTailRec(List("test", "of", "find", "function"), List("o", "f")) == List("of", "find", "function"))
    assert(L3.findManyTailRec(List("book", "256", "okay", "65", "76", "empty", "level", "frame", "elevator"), List("ok", "5", "ev")) == List("book", "256", "okay","65", "level", "elevator"))
  }

  test("Test of task 5: 'joinLists' function") {
    assert(L3.joinLists(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(L3.joinLists(List(1), Nil, List(2)) == List(1,2))
    assert(L3.joinLists(Nil, Nil, List("Smile", "!")) == List("Smile", "!"))
    assert(L3.joinLists(Nil, List("Have", "a"), List("nice", "day")) == List("Have", "a", "nice", "day"))
    assert(L3.joinLists(Nil, Nil, Nil) == Nil)
    assert(L3.joinLists(List("Have", "a"), List("very"), List("nice", "day")) == List("Have", "a", "very", "nice", "day"))
  }

  test("Test of task 5: 'joinListsTailRec' function") {
    assert(L3.joinListsTailRec(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(L3.joinListsTailRec(List(1), Nil, List(2)) == List(1,2))
    assert(L3.joinListsTailRec(Nil, Nil, List("Smile", "!")) == List("Smile", "!"))
    assert(L3.joinListsTailRec(Nil, List("Have", "a"), List("nice", "day")) == List("Have", "a", "nice", "day"))
    assert(L3.joinListsTailRec(Nil, Nil, Nil) == Nil)
    assert(L3.joinListsTailRec(List("Have", "a"), List("very"), List("nice", "day")) == List("Have", "a", "very", "nice", "day"))
  }
}

