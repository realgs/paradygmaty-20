import Tasks._
import scala.collection.immutable.Queue
import scala.collection.mutable

object Tests {

  def main(args: Array[String]): Unit = {

    // Task 1. tests
    assert(duplicate(Queue(), Queue()) == Queue())
    assert(duplicate(Queue(1), Queue(4)) == Queue(1, 1, 1, 1))
    assert(duplicate(Queue(1), Queue(4, 2)) == Queue(1, 1, 1, 1))
    assert(duplicate(Queue(), Queue(4, 2)) == Queue())
    assert(duplicate(Queue(3, 6), Queue()) == Queue())
    assert(duplicate(Queue(3, 6, -321, 3213), Queue(5, 7, 0, 2)) == Queue(3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 3213, 3213))

    // Task 2. tests
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(), Queue()) == Queue())
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(1), Queue(4)) == Queue(1, 1, 1, 1))
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(1), Queue(4, 2)) == Queue(1, 1, 1, 1))
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(), Queue(4, 2)) == Queue())
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(3, 6), Queue()) == Queue())
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(3, 6, -321, 3213), Queue(5, 7, 0, 2)) == Queue(3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 3213, 3213))
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(1, 1, 1), Queue(2, 3, 4)) == Queue(1, 1))
    assert(duplicateWithoutRepetitions(mutable.LinkedHashSet(1, 1, 2, 1, 4, 2), Queue(2, 3, 4, 5, 6, 7)) == Queue(1, 1, 2, 2, 2, 4, 4, 4, 4))

    // Task 3. tests
    val p = new Point(3, 4)
    p.debugName()
    val tc = new TestClass(1, -5.0, "TEST")
    tc.debugName()

    // Task 4. tests
    p.debugVars()
    tc.debugVars()

    // Task 5. tests
    assert(p.getClassName == "Point")
    assert(p.getClassFields == Map("x" -> (classOf[Int], 3), "y" -> (classOf[Int], 4), "a" -> (classOf[String], "test")))
    assert(tc.getClassName == "TestClass")
    assert(tc.getClassFields == Map("a" -> (classOf[Int], 1), "b" -> (classOf[Double], -5.0), "c" -> (classOf[String], "DEFAULT TEST STRING"), "d" -> (classOf[String], "TEST")))
  }
}
