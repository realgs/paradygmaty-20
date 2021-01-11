import Functions12.duplicate, Functions12.duplicateMod
import scala.collection.mutable

object Tests {
  def main(args: Array[String]): Unit =
    {
      zadanie1_tests()
      zadanie2_tests()
      zadanie3_tests()
      zadanie4_tests()
      zadanie5_tests()
    }

  def zadanie1_tests(): Unit =
  {
    println("Zadanie 1 - Testy")
    var queue = mutable.Queue[Int](1, 2, 3)
    var factors = List(0, 3, 1, 4)
    println(duplicate(queue, factors) == mutable.Queue(2, 2, 2, 3))

    queue = mutable.Queue[Int](1, 2, 3)
    factors = List(1, 2, 3)
    println(duplicate(queue, factors) == mutable.Queue(1, 2, 2, 3, 3, 3))

    queue = mutable.Queue()
    factors = List(3)
    println(duplicate(queue, factors) == mutable.Queue())

    queue = mutable.Queue(1, 2, 3)
    factors = List()
    println(duplicate(queue, factors) == mutable.Queue(1, 2, 3))
    println()
  }

  def zadanie2_tests(): Unit =
  {
    println("Zadanie 2 - Testy")
    var queue = mutable.Queue[Int](1, 2, 3)
    var factors = List(0, 3, 1, 4)
    println(duplicateMod(queue, factors)._1 == mutable.Queue(2, 2, 2, 3))

    queue = mutable.Queue[Int](1, 2, 3)
    factors = List(1, 2, 3)
    println(duplicateMod(queue, factors)._1 == mutable.Queue(1, 2, 2, 3, 3, 3))

    queue = mutable.Queue()
    factors = List(3)
    println(duplicateMod(queue, factors)._1 == mutable.Queue())

    queue = mutable.Queue(1, 2, 3)
    factors = List()
    println(duplicateMod(queue, factors)._1 == mutable.Queue(1, 2, 3))

    queue = mutable.Queue(1, 1, 2, 3)
    factors = List(3)
    println(duplicateMod(queue, factors)._1 == mutable.Queue(1, 1, 1, 2, 3))

    queue = mutable.Queue(3, 3, 3)
    factors = List(1)
    println(duplicateMod(queue, factors)._1 == mutable.Queue(3))

    queue = mutable.Queue(3, 3, 3, 4, 4, 4)
    factors = List(1, 2, 3)
    println(duplicateMod(queue, factors)._1 == mutable.Queue(3, 4, 4))
    println()
  }

  def zadanie3_tests(): Unit =
    {
      println("Zadanie 3 - Testy")
      val point = new Point(1, 2)
      point.debugName()
      println()
    }

  def zadanie4_tests(): Unit =
    {
      println("Zadanie 4 - Testy")
      val point = new Point(1, 2)
      point.debugVars()
      println()
    }

  def zadanie5_tests(): Unit =
    {
      println("Zadanie 5 - Testy")
      val point = new Point(1, 2)
      val fields: Fields = point.debugVars()
      val builder = new StringBuilder

      println("Names: ")
      println(fields.getNames.addString(builder, " ").toString)
      println()

      builder.clear()
      println("Types: ")
      println(fields.getTypes.map(t => t.toString).addString(builder, " "))
      println()

      builder.clear()
      println("Values: ")
      println(fields.getValues.map(t => t.toString).addString(builder, " "))
      println()

      builder.clear()
      println("ToString: ")
      println(fields.toString)
      println()
    }
}
