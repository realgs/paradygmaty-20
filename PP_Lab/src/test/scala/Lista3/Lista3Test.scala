package Lista3
import org.scalatest.FunSuite

class Lista3Test extends FunSuite {

  test("Lista 3 zadanie 1"){
    assert(Lista3.filterList(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(Lista3.filterList(List(0,0,0,0,0,0,0)) == (List(), List()))
    assert(Lista3.filterList(List(-1.2, -1.2, -2.0, 10)) == (List(-1.2, -1.2, -2.0), List(-1.2, -1.2)))
    assert(Lista3.filterList(List(1, 2, 3, 4, -5, 6, 7)) == (List(-5), List(-5)))
  }

  test("Lista 3 zadanie 2 test"){
    assert(Lista3.length(List(5,4,3,2)) == 4)
    assert(Lista3.length(List()) == 0)
    assert(Lista3.length(List(1)) == 1)
  }

  test("Lista 3 zadanie 3 test"){
    assert(Lista3.merge(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6))
    assert(Lista3.merge(List(), List()) == List())
    assert(Lista3.merge(List(), List(1,2,3,4,5,6)) == List(1,2,3,4,5,6))
    assert(Lista3.merge(List(5,4,3,2), List()) == List(5,4,3,2))
    assert(Lista3.merge(List('a', 'a', 'a', 'o', 'a'), List('l', 'm', 'k', 't')) == List('a', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'))
  }

  test("Lista 3 zadanie 5 test"){
    assert(Lista3.joinLists(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(Lista3.joinLists(List('a', 'l', 'a'), List('m', 'a'), List('k', 'o', 't', 'a')) == List('a', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'))
    assert(Lista3.joinLists(List(), List(), List("good", "job")) == List("good", "job"))
    assert(Lista3.joinLists(List(), List("good", "job"), List()) == List("good", "job"))
    assert(Lista3.joinLists(List("good", "job"), List(), List()) == List("good", "job"))
    assert(Lista3.joinLists(List("good", "job"), List(), List("have", "fun")) == List("good", "job", "have", "fun"))
  }
}
