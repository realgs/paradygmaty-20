import scala.collection.immutable.Queue
import Functions_L5.{duplicate, duplicateWithoutRepetition}

object Tests {

  def main(args: Array[String]): Unit = {
    task1Test()
    task2Test()
  }

  def task1Test(): Unit = {
    println("Task 1 test:")
    println(duplicate(Queue[Int](1, 2, 3), Queue[Int](0, 2, 3, 4)) == Queue(2, 2, 3, 3, 3))
    println(duplicate(Queue[Double](10.0, 14.66, 4.04, 88.1), Queue[Int](5, 1, 3)) == Queue(10.0, 10.0, 10.0, 10.0, 10.0, 14.66, 4.04, 4.04, 4.04))
    println(duplicate(Queue[String](), Queue[Int](10, 6, 3, 4)) == Queue())
    println(duplicate(Queue[Char]('a', 'b', 'c', 'd'), Queue[Int](0, 0, 0, 0)) == Queue())
    println(duplicate(Queue[Int](1, 2, 3), Queue[Int]()) == Queue())
    println(duplicate(Queue[Int](64, 5, 32, 5, 11), Queue[Int](1, 0, 0, 4)) == Queue(64, 5, 5, 5, 5))
  }

  def task2Test(): Unit = {
    println("Task 2 test:")
    println(duplicateWithoutRepetition(Set[Int](1, 1, 1), Queue[Int](0, 2, 3, 4)) == Queue())
    println(duplicateWithoutRepetition(Set[Double](10.1, 10.1, 4.04, 88.1), Queue[Int](5, 1, 3)) == Queue(10.1, 10.1, 10.1, 10.1, 10.1, 4.04, 88.1, 88.1, 88.1))
    println(duplicateWithoutRepetition(Set[String](), Queue[Int](10, 6, 3, 4)) == Queue())
    println(duplicateWithoutRepetition(Set[Char]('a', 'b', 'c', 'd'), Queue[Int](0, 0, 0, 0)) == Queue())
    println(duplicateWithoutRepetition(Set[Int](1, 2, 3), Queue[Int]()) == Queue())
    println(duplicateWithoutRepetition(Set[Int](64, 64, 32, 32, 11, 11), Queue[Int](1, 0, 0, 4)) == Queue(64))
  }
}
