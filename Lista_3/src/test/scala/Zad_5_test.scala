import org.scalatest.funsuite.AnyFunSuite

class Zad_5_test extends AnyFunSuite{

  val zad_5 = new Zad_5

  test("Test not empty list; linkThreeLists() function") {
    assert(zad_5.linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(zad_5.linkThreeLists(List(true, true), Nil, List(false, false, false)) == List(true, true, false, false, false))
    assert(zad_5.linkThreeLists(List(), List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
  }

  test("Test empty list; linkThreeLists() function") {
    assert(zad_5.linkThreeLists(Nil, Nil, Nil) == Nil)
    assert(zad_5.tailRec_linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }

  test("Test not empty list; tailRec_linkThreeLists() function") {
    assert(zad_5.tailRec_linkThreeLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(zad_5.tailRec_linkThreeLists(List(true, true), Nil, List(false, false, false)) == List(true, true, false, false, false))
    assert(zad_5.tailRec_linkThreeLists(List(), List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
  }

  test("Test empty list; tailRec_linkThreeLists() function") {
    assert(zad_5.tailRec_linkThreeLists(Nil, Nil, Nil) == Nil)
  }


}
