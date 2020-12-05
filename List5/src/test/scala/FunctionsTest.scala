import Functions.Debug
import org.scalatest.FunSuite

import scala.collection.immutable.Queue
import scala.util.Random

class FunctionsTest extends FunSuite {
  Random.setSeed(0)

  val LONG_LENGTH = 100000

  val longCollection: Queue[Int] = Queue.fill(LONG_LENGTH)(Random.nextInt)
  val longMask: Queue[Int] = Queue.fill(LONG_LENGTH)(Random.between(0, 2000))

  def timeit[R](block: => R): Unit = {
    val start = System.nanoTime()
    block
    val end = System.nanoTime()
    printf("Executed block in: %sns\n", end - start)
  }

  test("duplicate.testSpeed") {
    timeit(Functions.duplicate(longCollection)(longMask))
  }

  test("duplicate.bothEmpty") {
    val collection = Array()
    val mask = Array()
    assert(Functions.duplicate(collection)(mask) === Array())
  }

  test("duplicate.maskEmpty") {
    val collection = Array(1, 4, 8, 2, 1, 8)
    val mask = Array()
    assert(Functions.duplicate(collection)(mask) === Array(1, 4, 8, 2, 1, 8))
  }

  test("duplicate.collectionEmpty") {
    val collection = Seq()
    val mask = Seq(1, 0, 2, 7)
    assert(Functions.duplicate(collection)(mask) === Seq())
  }

  test("duplicate.example") {
    val collection = List(1, 2, 3)
    val mask = List(0, 3, 1, 4)
    assert(Functions.duplicate(collection)(mask) === List(2, 2, 2, 3))
  }

  test("duplicate.negativeMultiplier") {
    val collection = List(1, 2, 3, 4)
    val mask = List(1, 3, -1, 0)
    assert(Functions.duplicate(collection)(mask) === List(1, 2, 2, 2))
  }

  test("duplicate.maskShorter") {
    val collection = List(1, 2, 3, 4)
    val mask = List(2, 1)
    assert(Functions.duplicate(collection)(mask) === List(1, 1, 2, 3, 4))
  }

  test("duplicate.maskLonger") {
    val collection = List(1, 2, 3, 4)
    val mask = List(2, 1, 3, 1, 0, 5, 2, 8)
    assert(Functions.duplicate(collection)(mask) === List(1, 1, 2, 3, 3, 3, 4))
  }

  test("duplicateDistinct.example") {
    val collection = List(1, 2, 2, 4)
    val mask = List(2, 1, 3)
    assert(Functions.duplicateDistinct(collection)(mask) === List(1, 1, 2, 4, 4, 4))
  }

  test("duplicateDistinct.sameElements") {
    val collection = List(5, 5, 5, 5, 5)
    val mask = List(4, 1, 7, 8, 3)
    assert(Functions.duplicateDistinct(collection)(mask) === List(5, 5, 5, 5))
  }

  test("duplicateDistinct.emptyMask") {
    val collection = List(5, 5, 5, 5, 5)
    val mask = List()
    assert(Functions.duplicateDistinct(collection)(mask) === List(5))
  }

  test("duplicateDistinct.emptyCollection") {
    val collection = List()
    val mask = List(1, 2, 3)
    assert(Functions.duplicateDistinct(collection)(mask) === List())
  }

  // Example class for testing Debug trait
  class Point extends Debug {
    var x, y = 0
    var name = ""
  }

  object Point {
    def apply(x: Int, y: Int)(name: String): Point = {
      val p = new Point
      p.x = x
      p.y = y
      p.name = name
      p
    }
  }

  val p1: Point = Point(1, 5)("point1")
  val p2: Point = Point(3, 7)("point2")

  test("debugName.nullReference") {
    val ref: Point = null
    assertThrows[NullPointerException] {
      ref.debugName()
    }
  }

  test("debugName.example") {
    val expected = Map("x" -> Array("1", "int"), "y" -> Array("5", "int"), "name" -> Array("point1", "String"))
    val actual = p1.getDebugVarsMap
    assert(expected("x") sameElements actual("x"))
    assert(expected("y") sameElements actual("y"))
    assert(expected("name") sameElements actual("name"))
  }

  test("args.nullReference") {
    val ref: Point = null
    assertThrows[NullPointerException] {
      ref.debugVars()
    }
  }

  test("args.example") {
    val expected = Map("x" -> Array("3", "int"), "y" -> Array("7", "int"), "name" -> Array("point2", "String"))
    val actual = p2.getDebugVarsMap
    assert(expected("x") sameElements actual("x"))
    assert(expected("y") sameElements actual("y"))
    assert(expected("name") sameElements actual("name"))
  }
}
