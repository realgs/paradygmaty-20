import L3Lab.{doubleListSplitBy, listLength, listMergeWith, listMergeWithTail, listZipAlternately, stringListSearch, stringListSearchList, stringListSearchListTail, stringListSearchTail}
import org.scalatest.FunSuite

class L3LabTest extends FunSuite {
  test("Task 1") {
    def takeNegatives(value: Double) = value < 0

    def takeOddsThatAreNegatives(value: Double) = takeNegatives(value) && (value % 2 != 0)

    assert(doubleListSplitBy(List(-1, -2, 3, -4, -5), takeNegatives, takeOddsThatAreNegatives) == (List(-1, -2, -4, -5), List(-1, -5)))
    assert(doubleListSplitBy(List(1, 2, 3, 4, -5), takeNegatives, takeOddsThatAreNegatives) == (List(-5), List(-5)))
    assert(doubleListSplitBy(List(-2.0, -1.4, -2.0, 1.4), takeNegatives, takeOddsThatAreNegatives) == (List(-2.0, -1.4, -2.0), List(-1.4)))
    assert(doubleListSplitBy(List(0), takeNegatives, takeOddsThatAreNegatives) == (List(), List()))
    assert(doubleListSplitBy(List(), takeNegatives, takeOddsThatAreNegatives) == (List(), List()))
  }

  test("Task 2") {
    assert(listZipAlternately(List(1, 3, 5, 7, 9), List(2, 4, 6, 8, 10)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(listZipAlternately(List("ala", "psa"), List("ma")) == List("ala", "ma", "psa"))
    assert(listZipAlternately(List(), List(1, 2, 3)) == List(1, 2, 3))
    assert(listZipAlternately(List(1, 2, 3), List()) == List(1, 2, 3))
    assert(listZipAlternately(List(), List()) == List())
  }

  test("Task 3") {
    assert(listLength(List(1, 2, 3)) == 3)
    assert(listLength(List(0)) == 1)
    assert(listLength(List()) == 0)
  }

  test("Task 4") {
    assert(stringListSearch(List("ala", "ma", "ale", "i", "psa"), "al") == List("ala", "ale"))
    assert(stringListSearch(List("to", "mozliwe"), "al") == List())
    assert(stringListSearch(List("to", "mozliwe"), "") == List("to", "mozliwe"))

    assert(stringListSearchTail(List("ala", "ma", "ale", "i", "psa"), "al") == List("ala", "ale"))
    assert(stringListSearchTail(List("to", "mozliwe"), "al") == List())
    assert(stringListSearchTail(List("to", "mozliwe"), "") == List("to", "mozliwe"))

    assert(stringListSearchList(List("ala", "ma", "kota", "i", "psa"), List("al", "ma", "ota")) == List("ala", "ma", "kota"))
    assert(stringListSearchList(List("to", "mozliwe"), List("al")) == List())
    assert(stringListSearchList(List("to", "mozliwe"), List()) == List())

    assert(stringListSearchListTail(List("ala", "ma", "kota", "i", "psa"), List("al", "ma", "ota")) == List("ala", "ma", "kota"))
    assert(stringListSearchListTail(List("to", "mozliwe"), List("al")) == List())
    assert(stringListSearchListTail(List("to", "mozliwe"), List()) == List())
  }

  test("Task 5") {
    assert(listMergeWith(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(listMergeWith(List('*', '*', '*', '*', '*'), List(' '), List('*', '*', '*')) == List('*', '*', '*', '*', '*', ' ', '*', '*', '*'))
    assert(listMergeWith(List("ala", "ma"), List(), List()) == List("ala", "ma"))
    assert(listMergeWith(List(), List("ala", "ma"), List()) == List("ala", "ma"))
    assert(listMergeWith(List(), List(), List("ala", "ma")) == List("ala", "ma"))
    assert(listMergeWith(List("ala", "ma"), List(), List("kota")) == List("ala", "ma", "kota"))

    assert(listMergeWithTail(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(listMergeWithTail(List('*', '*', '*', '*', '*'), List(' '), List('*', '*', '*')) == List('*', '*', '*', '*', '*', ' ', '*', '*', '*'))
    assert(listMergeWithTail(List("ala", "ma"), List(), List()) == List("ala", "ma"))
    assert(listMergeWithTail(List(), List("ala", "ma"), List()) == List("ala", "ma"))
    assert(listMergeWithTail(List(), List(), List("ala", "ma")) == List("ala", "ma"))
    assert(listMergeWithTail(List("ala", "ma"), List(), List("kota")) == List("ala", "ma", "kota"))
  }
}
