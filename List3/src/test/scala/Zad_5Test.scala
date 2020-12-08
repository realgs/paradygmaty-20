import org.scalatest.funsuite.AnyFunSuite

class Zad_5Test extends AnyFunSuite{

  val test = new Zad_5

  test("Test not empty list; linkThreeLists() function") {
    assert(test.linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(test.linkThreeLists(List(true, true), Nil, List(false, false, false)) == List(true, true, false, false, false))
    assert(test.linkThreeLists(List(), List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
  }

  test("Test empty list; linkThreeLists() function") {
    assert(test.linkThreeLists(Nil, Nil, Nil) == Nil)
    assert(test.tailRec_linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }

  test("Test not empty list; tailRec_linkThreeLists() function") {
    assert(test.tailRec_linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(test.tailRec_linkThreeLists(List(true, true), Nil, List(false, false, false)) == List(true, true, false, false, false))
    assert(test.tailRec_linkThreeLists(List(), List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
  }

  test("Test empty list; tailRec_linkThreeLists() function") {
    assert(test.tailRec_linkThreeLists(Nil, Nil, Nil) == Nil)
  }

}