import scala.collection.immutable.Queue
import TestClasses.{Point, Point3D, Point4D, Animal, Dog}
import Functions_L5.{duplicate, duplicateWithoutRepetition}

object Tests {

  def main(args: Array[String]): Unit = {
    task1Test()
    task2Test()
    task3Test()
    task4Test()
    task5Test()
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
    println("\nTask 2 test:")
    println(duplicateWithoutRepetition(Set[Int](1, 1, 1), Queue[Int](0, 2, 3, 4)) == Queue())
    println(duplicateWithoutRepetition(Set[Double](10.1, 10.1, 4.04, 88.1), Queue[Int](5, 1, 3)) == Queue(10.1, 10.1, 10.1, 10.1, 10.1, 4.04, 88.1, 88.1, 88.1))
    println(duplicateWithoutRepetition(Set[String](), Queue[Int](10, 6, 3, 4)) == Queue())
    println(duplicateWithoutRepetition(Set[Char]('a', 'b', 'c', 'd'), Queue[Int](0, 0, 0, 0)) == Queue())
    println(duplicateWithoutRepetition(Set[Int](1, 2, 3), Queue[Int]()) == Queue())
    println(duplicateWithoutRepetition(Set[Int](64, 64, 32, 32, 11, 11), Queue[Int](1, 0, 0, 4)) == Queue(64))
  }

  def task3Test(): Unit = {
    println("\nTask 3 test:")
    val p: Point = new Point(3, 4)
    val p1: Point3D = new Point3D(3, 4, 5)
    val p2: Point4D = new Point4D(3, 4, 5, 6)
    val dog: Dog = new Dog("doggo", 3, "pug")

    p.printName()
    p1.printName()
    p2.printName()
    dog.printName()
  }

  def task4Test(): Unit = {
    println("\nTask 4 test:")
    val p: Point = new Point(3, 4)
    val p1: Point3D = new Point3D(3, 4, 5)
    val p2: Point4D = new Point4D(3, 4, 5, 6)
    val animal: Animal = new Animal("animal", 5)
    val dog: Dog = new Dog("doggo", 3, "pug")

    p.printVars()
    println()
    p1.printVars()
    println()
    p2.printVars()
    println()
    animal.printVars()
    println()
    dog.printVars()
  }

  def task5Test(): Unit = {
    println("\nTask 5 tests:")
    val p: Point = new Point(3, 4)
    val p1: Point3D = new Point3D(3, 4, 5)
    val p2: Point4D = new Point4D(3, 4, 5, 6)
    val animal: Animal = new Animal("animal", 5)
    val dog: Dog = new Dog("doggo", 3, "pug")

    println(p.debugName())//"Point"
    printMap(p.debugVars())
    println()
    println(p1.debugName())//"Point3D"
    printMap(p1.debugVars())
    println()
    println(p2.debugName())//"Point4D"
    printMap(p2.debugVars())
    println()
    println(animal.debugName())//"Animal"
    printMap(animal.debugVars())
    println()
    println(dog.debugName())//"Dog"
    printMap(dog.debugVars())
  }

  def printMap(map: Map[String, (Class[_], Any)]): Unit = {
    for (element <- map) {
      println("Var: " + element._1 + " => " + element._2._1 + ", " + element._2._2)
    }
  }
}
