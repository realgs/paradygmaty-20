import org.scalatest.FunSuite

class ConcatenateListsTest extends FunSuite {
   test("normalList") {
     assert(L3.concatenateLists(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6))
   }

  test("normalStringList") {
    assert(L3.concatenateLists(List("*****", "i"), List("***", "konfederacje")) == List("*****","***","i","konfederacje"))
  }

  test("oneEmptyList") {
    assert(L3.concatenateLists(List(5,4,3,2), Nil) == List(5,4,3,2))
  }

  test("twoEmptyLists") {
    assert(L3.concatenateLists(Nil, Nil) == Nil)
  }

}
