import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class DuplicateCollectionTest extends AnyFunSuite {

  val testDupl = new DuplicateCollection

  //Zadanie 1
  test("Test fst collection length > snd collection length; duplicate function") {
    val q1 = mutable.Queue[Int](6, 11, 5, 6, 4, 4, 6)
    val q2 = mutable.Queue[Int](2, 0, 4, 1)
    assert(testDupl.duplicate(q1, q2) == mutable.Queue(6, 6, 5, 5, 5, 5, 6))
  }

  test("Test fst collection length = snd collection length; duplicate function") {
    val q1 = mutable.Queue[String]("a", "b", "c")
    val q2 = mutable.Queue[Int](2, 0, 4)
    assert(testDupl.duplicate(q1, q2) == mutable.Queue("a", "a", "c", "c", "c", "c"))
  }

  test("Test fst collection length < snd collection length; duplicate function") {
    val q1 = mutable.Queue[Char]('x', 'y', 'z')
    val q2 = mutable.Queue[Int](2, 0, 3, 5, 9)
    assert(testDupl.duplicate(q1, q2) == mutable.Queue('x', 'x', 'z', 'z', 'z'))
  }

  test("Test fst collection empty; duplicate function") {
    val q1 = mutable.Queue[String]()
    val q2 = mutable.Queue[Int](2, 0, 4)
    assert(testDupl.duplicate(q1, q2) == mutable.Queue())
  }

  test("Test snd collection empty; duplicate function") {
    val q1 = mutable.Queue[String]("a", "b", "c")
    val q2 = mutable.Queue[Int]()
    assert(testDupl.duplicate(q1, q2) == mutable.Queue())
  }

  test("Test both collections empty; duplicate function") {
    val q1 = mutable.Queue[String]()
    val q2 = mutable.Queue[Int]()
    assert(testDupl.duplicate(q1, q2) == mutable.Queue())
  }

  //Zadanie 2
  test("Test fst collection length > snd collection length; duplicateSet function") {
    val q1 = Set[Int](6, 11, 5, 4, 4, 6)
    val q2 = mutable.Queue[Int](2, 0, 4)
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue(6, 6, 5, 5, 5, 5))
  }

  test("Test fst collection length = snd collection length; duplicateSet function") {
    val q1 = Set[String]("a", "b", "c")
    val q2 = mutable.Queue[Int](2, 0, 4)
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue("a", "a", "c", "c", "c", "c"))
  }

  test("Test fst collection length < snd collection length; duplicateSet function") {
    val q1 = Set[Char]('x', 'y', 'z')
    val q2 = mutable.Queue[Int](2, 0, 3, 5, 9)
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue('x', 'x', 'z', 'z', 'z'))
  }

  test("Test duplicates in fst collection; duplicateSet function") {
    val q1 = Set[Int](6, 11, 5, 6, 4, 4, 6)
    val q2 = mutable.Queue[Int](2, 0, 3, 2)
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue(6, 6, 5, 5, 5, 4, 4))
  }

  test("Test fst collection empty; duplicateSet function") {
    val q1 = Set[String]()
    val q2 = mutable.Queue[Int](2, 0, 4)
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue())
  }

  test("Test snd collection empty; duplicateSet function") {
    val q1 = Set[String]("a", "b", "c")
    val q2 = mutable.Queue[Int]()
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue())
  }

  test("Test both collections empty; duplicateSet function") {
    val q1 = Set[String]()
    val q2 = mutable.Queue[Int]()
    assert(testDupl.duplicateSet(q1, q2) == mutable.Queue())
  }

}
