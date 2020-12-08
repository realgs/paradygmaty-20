import org.scalatest.funsuite.AnyFunSuite

class Zad_1_2_3Test extends AnyFunSuite{

  val test = new Zad_1_2_3

  //zadanie 1
  test("Test not empty lists; findNegativeNumbers() function") {
    assert(test.findNegativeNumbers(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(test.findNegativeNumbers(List(0, 1, 2, 3)) == (Nil, Nil))
    assert(test.findNegativeNumbers(List(-4, -6, -10)) == (List(-4, -6, -10), Nil))
    assert(test.findNegativeNumbers(List(-3, -5, -7)) == (List(-3, -5, -7), List(-3, -5, -7)))
  }

  test("Test empty list; findNegativeNumbers() function") {
    assert(test.findNegativeNumbers(Nil) == (Nil, Nil))
  }

  //zadanie 2
  test("Test not empty list; findLength() function") {
    assert(test.findLength(List(5, 4, 3, 2)) == 4)
    assert(test.findLength(List(true, false, false)) == 3)
    assert(test.findLength(List("Ala ma", " kota.")) == 2)
  }

  test("Test empty list; findLength() function") {
    assert(test.findLength(Nil) == 0)
  }

  //zadanie 3
  test("Test not empty list; mixLists() function") {
    assert(test.mixLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
    assert(test.mixLists(List("Ala", "burego"), List("ma", "kota")) == List("Ala", "ma", "burego", "kota"))
    assert(test.mixLists(List(true, true, false), Nil) == List(true, true, false))
  }

  test("Test empty list; mixLists() function") {
    assert(test.mixLists(Nil, Nil) == Nil)
  }

}
