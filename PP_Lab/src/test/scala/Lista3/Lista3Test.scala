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


  test("Lista 3 zadanie 4 test"){
    assert(Lista3.stringLength("Ala ma kota") == 11)
    assert(Lista3.stringLength("") == 0)
    assert(Lista3.stringLength("  ") == 2)
    assert(Lista3.stringLength("jakiswyraz") == 10)

    assert(Lista3.sliceStringFromBegin("Ala ma kota", 5) == ("Ala ma", " kota"))
    assert(Lista3.sliceStringFromBegin("Ala ma kota",0) == ("A", "la ma kota"))
    assert(Lista3.sliceStringFromBegin("trolek", 3) == ("trol", "ek"))
    assert(Lista3.sliceStringFromBegin("kot",  2) == ("kot", ""))

    assert(Lista3.ifContainsKey("bufor", "for"))
    assert(!Lista3.ifContainsKey("for", "bufor"))
    assert(Lista3.ifContainsKey("ala ma kota", "ala ma kota"))
    assert(Lista3.ifContainsKey("ala ma kota", "kota"))
    assert(Lista3.ifContainsKey("ala ma kota", "ma kota"))
    assert(Lista3.ifContainsKey("notak", "tak"))

    assert(Lista3.doesWordMatchAnyKey("notak", List("rak", "opak", "tak", "ok")))
    assert(Lista3.doesWordMatchAnyKey("alpaka", List("mak", "tak", "paka", "ok")))
    assert(Lista3.doesWordMatchAnyKey("ok", List("rak", "opak", "tak", "ok")))
    assert(!Lista3.doesWordMatchAnyKey("olej", List("rak", "opak", "tak", "ok")))

    assert(Lista3.find(List("ala", "ma", "kota", "mama"), List("al", "ma", "kto", "amam")) == List("ala", "ma", "mama"))
    assert(Lista3.find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210"))
    assert(Lista3.find(List("alek", "ma", "kota", "mama"), List("ala", "amam")) == List())

    assert(Lista3.findTail(List("ala", "ma", "kota", "mama"), List("al", "ma", "kto", "amam")) == List("ala", "ma", "mama"))
    assert(Lista3.findTail(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210"))
    assert(Lista3.findTail(List("alek", "ma", "kota", "mama"), List("ala", "amam")) == List())
  }


  test("Lista 3 zadanie 5 test"){
    assert(Lista3.joinLists(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(Lista3.joinLists(List('a', 'l', 'a'), List('m', 'a'), List('k', 'o', 't', 'a')) == List('a', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'))
    assert(Lista3.joinLists(List(), List(), List("good", "job")) == List("good", "job"))
    assert(Lista3.joinLists(List(), List("good", "job"), List()) == List("good", "job"))
    assert(Lista3.joinLists(List("good", "job"), List(), List()) == List("good", "job"))
    assert(Lista3.joinLists(List("good", "job"), List(), List("have", "fun")) == List("good", "job", "have", "fun"))

    assert(Lista3.appendList(List(1,2,3), List(5,6))== List(1,2,3,5,6))
    assert(Lista3.appendList(List('a', 'l', 'a', ' '), List('o', 'l', 'a'))== List('a', 'l', 'a', ' ', 'o', 'l', 'a'))

    assert(Lista3.joinListsTail(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    assert(Lista3.joinListsTail(List('a', 'l', 'a'), List('m', 'a'), List('k', 'o', 't', 'a')) == List('a', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a'))
    assert(Lista3.joinListsTail(List(), List(), List("good", "job")) == List("good", "job"))
    assert(Lista3.joinListsTail(List(), List("good", "job"), List()) == List("good", "job"))
    assert(Lista3.joinListsTail(List("good", "job"), List(), List()) == List("good", "job"))
    assert(Lista3.joinListsTail(List("good", "job"), List(), List("have", "fun")) == List("good", "job", "have", "fun"))
  }
}
