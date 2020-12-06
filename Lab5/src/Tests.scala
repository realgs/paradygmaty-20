import Tasks._
import scala.collection.immutable.Queue

object Tests {

  def main(args: Array[String]): Unit = {

    // Task 1. tests
    println(duplicate(Queue(), Queue()) == Queue())
    println(duplicate(Queue(1), Queue(4)) == Queue(1, 1, 1, 1))
    println(duplicate(Queue(1), Queue(4, 2)) == Queue(1, 1, 1, 1))
    println(duplicate(Queue(), Queue(4, 2)) == Queue())
    println(duplicate(Queue(3, 6), Queue()) == Queue())
    println(duplicate(Queue(3, 6, -321, 3213), Queue(5, 7, 0, 2)) == Queue(3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 3213, 3213))

    // Task 2. tests
    println(duplicateWithoutRepetitions(Set(), Queue()) == Queue())
    println(duplicateWithoutRepetitions(Set(1), Queue(4)) == Queue(1, 1, 1, 1))
    println(duplicateWithoutRepetitions(Set(1), Queue(4, 2)) == Queue(1, 1, 1, 1))
    println(duplicateWithoutRepetitions(Set(), Queue(4, 2)) == Queue())
    println(duplicateWithoutRepetitions(Set(3, 6), Queue()) == Queue())
    println(duplicateWithoutRepetitions(Set(3, 6, -321, 3213), Queue(5, 7, 0, 2)) == Queue(3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 3213, 3213))
    println(duplicateWithoutRepetitions(Set(1, 1, 1), Queue(2, 3, 4)) == Queue(1, 1))
    println(duplicateWithoutRepetitions(Set(1, 1, 2, 1, 4, 2), Queue(2, 3, 4, 5, 6, 7)) == Queue(1, 1, 2, 2, 2, 4, 4, 4, 4))

    // Task 3. tests
    val p = new Point(3, 4)
    p.debugName()

    // Task 4. tests
    p.debugVars()

    // Task 5. tests
    println(p.getClassName)
    println(p.getClassFields)
  }
}
