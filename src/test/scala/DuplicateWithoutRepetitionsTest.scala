import org.scalatest.FunSuite

import scala.collection.immutable.Queue
import scala.collection.mutable.LinkedHashSet

// zadanie 2 testy (2,5 pkt)
class DuplicateWithoutRepetitionsTest extends FunSuite {

  test("normalTest") {
    assert(Lista5.duplicateWithoutRepetitions(LinkedHashSet(1,1,1,2,2,2,3,3,3), Queue(0,3,1,4)) == Queue(2,2,2,3))
  }

  test("stringList") {
    assert(Lista5.duplicateWithoutRepetitions(LinkedHashSet("scala", "ocaml", "ocaml", "scala", "haskell"), Queue(0,2,1)) == Queue("ocaml", "ocaml", "haskell"))
  }

  test("emptyDuplicatesTimesList") {
    assert(Lista5.duplicateWithoutRepetitions(LinkedHashSet(1,2,3), Queue()) == Queue())
  }

}
