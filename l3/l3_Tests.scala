import org.scalatest.FunSuite

//Zadanie 1 testy
class l3_listSplit_Test extends FunSuite {
  test("listSplit_List_example") {
    assert(l3.listSplit(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
  }
  test("listSplit_Listof9elem") {
    assert(l3.listSplit(List(-3, -6, -9, 3, -11, -4, 12, 0, -2)) == (List(-3, -6, -9, -11, -4, -2), List(-3, -9, -11)))
  }
  test("listSplit_List_all_non-negative") {
    assert(l3.listSplit(List(3, 11, 4, 12, 0, 2, 64)) == (List(), List()))
  }
}

//Zadanie 2 testy
class l3_listLength_Test extends FunSuite {
  test("listLength_Listof3elem") {
    assert(l3.listLength(List(1, 2, 3)) == 3)
  }
  test("listLength_Alamakota") {
    assert(l3.listLength(List("a", "l", "a", "m", "a", "k", "o", "t", "a")) == 9)
  }
  test("listLength_Listof9elem") {
    assert(l3.listLength(List(3, 4, 1, 1, 2, 1, 8, 8, 8, 8, 8, 8)) == 12)
  }
}

//Zadanie 3 testy
class l3_connect_Test extends FunSuite{
  test("connect_List_List_Int") {
    assert(l3.connect(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6, 7, 8, 9))
  }
  test("connect_List_List_Double") {
    assert(l3.connect(List(2.5, 4.6, 5.5, 6.0), List(1.0, 3.1, 7.8, 8.2, 7.7)) == List(2.5, 1.0, 4.6, 3.1, 5.5, 7.8, 6.0, 8.2, 7.7))
  }
  test("connect_List_List_Char") {
    assert(l3.connect(List('a', 'c'), List('b', 'd', 'e', 'f')) == List('a', 'b', 'c','d', 'e', 'f'))
  }
  test("connect_Nil_List") {
    assert(l3.connect(List(), List(2, 4, 5, 6)) == List(2, 4, 5, 6))
  }
  test("connect_Nil_Nil") {
    assert(l3.connect(List(), List()) == List())
  }
}

//Zadanie 4 nieogonowa testy
class l3_find_Test extends FunSuite {
  test("find_List_Int") {
    assert(l3_Task4.find(List(111, 212, 30301, 20), List(1, 2)) == List(111, 212, 30301, 20))
  }
  test("find_List_Example") {
    assert(l3_Task4.find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168"))
      == List("index0168202", "index0168211", "index0168210"))
  }
  test("find_List_Double") {
    assert(l3_Task4.find(List(1.0, 2.9, 37.92, 2.3), List(2)) == List(2.9, 37.92, 2.3))
  }
}

//Zadanie 4 ogonowa testy
class l3_findTailrec_Test extends FunSuite {
  test("findTailrec_List_Int") {
    assert(l3_Task4.findTailrec(List(111, 212, 30301, 20), List(1, 2)) == List(111, 212, 30301, 20))
  }
  test("findTailrec_List_Example") {
    assert(l3_Task4.findTailrec(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168"))
      == List("index0168202", "index0168211", "index0168210"))
  }
  test("findTailrec_List_Double") {
    assert(l3_Task4.findTailrec(List(1.0, 2.9, 37.92, 2.3), List(2)) == List(2.9, 37.92, 2.3))
  }
}

//Zadanie 5 testy
class l3_joinLists_Test extends FunSuite {
  test("joinLists_List_Int") {
    assert(l3.joinLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
  test("joinLists_List_List_Char") {
    assert(l3.joinLists(List('a', 'c'), List('b', 'd'), List('e', 'f')) == List('a', 'c', 'b','d', 'e', 'f'))
  }
  test("joinLists_Nil_List_Nil") {
    assert(l3.joinLists(List(), List(2, 4, 5, 6), Nil) == List(2, 4, 5, 6))
  }
}


