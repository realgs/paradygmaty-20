import org.scalatest.FunSuite

class LengthTest extends FunSuite{

  test("integerList") {
    assert(L3.length(List(1,2,3,4,5)) == 5)
  }

  test("stringList") {
    assert(L3.length(List("*****","***")) == 2)
  }

  test("oneElementList") {
    assert(L3.length(List(1)) == 1)
  }

  test("emptyList") {
    assert(L3.length(Nil) == 0)
  }

}
