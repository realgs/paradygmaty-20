import org.scalatest.FunSuite

import scala.collection.immutable.Queue

// zadanie 1 testy (2,5 pkt)
class DuplicateTest extends FunSuite {

  test("normalTest") {
    assert(Lista5.duplicate(List(1,2,3),List(0,3,1,2)) == Queue(2,2,2,3))
  }

  test("normalTest2") {
    assert(Lista5.duplicate(List(1,2,3,4,5), List(1,3,1,2)) == Queue(1,2,2,2,3,4,4))
  }

  test("emptyDuplicatesTimesList") {
    assert(Lista5.duplicate(List(1,2,3), List()) == Queue())
  }

  test("stringList") {
    assert(Lista5.duplicate(List("scala", "ocaml", "haskell"), List(0,2,1)) == Queue("ocaml", "ocaml", "haskell"))
  }

  test("negativeDuplicatesTimes") {
    assertThrows[IllegalArgumentException](Lista5.duplicate(List(1.0, 2.0),List(5,-6)))
  }
}
