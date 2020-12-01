import org.scalatest.FunSuite

class Lista3Test extends FunSuite {
  //tests for 1 task
  test("Lista3Test.devide"){
    assert(lista3.Zadanie_1.devide(List()) == (Nil,Nil))
    assert(lista3.Zadanie_1.devide(List(4, 6, 9, 8, 17, 19, 0)) == (Nil, Nil))
    assert(lista3.Zadanie_1.devide(List(-1, -4, -7, 9, -4, 6, -10, -101)) == ((List(-1, -4, -7, -4, -10, -101), List(-1, -7, -101))))
  }

  //tests for 2 task

  test("Lista3Test.lengthTai"){
    assert(lista3.Zadanie_2.lengthTail(List()) == 0)
    assert(lista3.Zadanie_2.lengthTail(List(1, 2, 3, 4, 5)) == 5)
    assert(lista3.Zadanie_2.lengthTail(List("I", "like", "apples")) == 3)
  }

  //tests for 3 task
  test("Lista3Test.connect"){
    assert(lista3.Zadanie_3.connect(List(),List())  == List())
    assert(lista3.Zadanie_3.connect(List(1, 3, 5),List(2, 4, 6))  == List(1, 2, 3, 4, 5, 6))
    assert(lista3.Zadanie_3.connect(List("I", "apples", "bananas"),List("like", "peaches"))  == List("I", "like", "apples", "peaches", "bananas"))
    assert(lista3.Zadanie_3.connect(List("I", 100, "programming"),List("like", 500))  == List("I", "like", 100, 500, "programming"))
  }

  //tests for 4 task
  test("Lista3Test.find"){
    assert(lista3.Zadanie_4.find(List(),"hello") == Nil)
    assert(lista3.Zadanie_4.find(List("hello","how", "are", "you" ),"") == Nil)
    assert(lista3.Zadanie_4.find(List("qwertmom","as12momdfg","ghjkljhg","mom","12","12345612mom"),"12mom")==List("as12momdfg", "12345612mom"))
  }

  test("Lista3Test.findTail"){
    assert(lista3.Zadanie_4.taiRecFind(List(),"hello") == Nil)
    assert(lista3.Zadanie_4.taiRecFind(List("hello","how", "are", "you" ),"") == Nil)
    assert(lista3.Zadanie_4.taiRecFind(List("qwertmom","as12momdfg","ghjkljhg","mom","12","12345612mom"),"12mom")==List("as12momdfg", "12345612mom"))
  }

  //tests for 5 task
  test("Lista3Test.joinLists"){
    assert(lista3.Zadanie_5.joinListsRec(List(1,2,3,4),List(5,6,7), List(8,9))==List(1,2,3,4,5,6,7,8,9))
    assert(lista3.Zadanie_5.joinListsRec(List(1,2,3,4),List(5,6,7), List())==List(1,2,3,4,5,6,7))
    assert(lista3.Zadanie_5.joinListsRec(List(1,2,3,4),List(), List(8,9))==List(1,2,3,4,8,9))
    assert(lista3.Zadanie_5.joinListsRec(List(),List(), List())==List())
    assert(lista3.Zadanie_5.joinListsRec(List(1,2,3,4),List("Alla","ma", "kota"), List(8,9))==List(1,2,3,4,"Alla","ma","kota",8,9))
  }

  test("Lista3Test.joinListsTail") {
    assert(lista3.Zadanie_5.joinListsTail(List(1, 2, 3, 4), List(5, 6, 7), List(8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(lista3.Zadanie_5.joinListsTail(List(1, 2, 3, 4), List(5, 6, 7), List()) == List(1, 2, 3, 4, 5, 6, 7))
    assert(lista3.Zadanie_5.joinListsTail(List(1, 2, 3, 4), List(), List(8, 9)) == List(1, 2, 3, 4, 8, 9))
    assert(lista3.Zadanie_5.joinListsTail(List(), List(), List()) == List())
    assert(lista3.Zadanie_5.joinListsTail(List(1, 2, 3, 4), List("Alla", "ma", "kota"), List(8, 9)) == List(1, 2, 3, 4, "Alla", "ma", "kota", 8, 9))
  }
}
