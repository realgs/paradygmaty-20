import org.scalatest.FunSuite

class EachNElementTest extends FunSuite {

  val mockLazyList1 = LazyList(5,6,3,2,1)
  val mockLazyList2 = LazyList(1,2,3,4,5,6,7,8,9,10,11,12)

  test("basicTest1") {
    assert(Lista4.eachNElement(mockLazyList1,2,3).toList == List(5,3))
  }

  test("basicTest2") {
    assert(Lista4.eachNElement(mockLazyList1,2,4).toList == List(5,3))
  }

  test("basicTest3") {
    assert(Lista4.eachNElement(mockLazyList2,3,9).toList == List(1,4,7))
  }

  test("each0Element") {
    assert(Lista4.eachNElement(mockLazyList1,0,4) == LazyList())
  }

  test("each1Element") {
    assert(Lista4.eachNElement(mockLazyList1,1,4) == mockLazyList1)
  }

  test("negativeParameter") {
    assertThrows[IllegalArgumentException](Lista4.eachNElement(mockLazyList1,-10, 4))
  }
}
