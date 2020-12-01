package lab5

import java.lang.reflect.AnnotatedElement

import ClassesForTest.{Car, Point, Rectangle}
import org.junit.jupiter.api.Test

import scala.collection.immutable.Queue

class Tests {
  val testFunction: Functions = new Functions

  @Test
  def testFunction1 = {
    assert(testFunction.duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(testFunction.duplicate(Queue(), Queue(1, 2, 3)) == Queue())
    assert(testFunction.duplicate(Queue('b', 'a', 'b'), Queue(0, 0, 3)) == Queue('b', 'b', 'b'))
  }

  @Test
  def testFunction2 = {
    assert(testFunction.duplicateWithNoReps(Set(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(testFunction.duplicateWithNoReps(Set(), Queue(1, 2, 3)) == Queue())
    assert(testFunction.duplicateWithNoReps(Set('b', 'a', 'c'), Queue(0, 0, 3)) == Queue('c', 'c', 'c'))
    assert(testFunction.duplicateWithNoReps(Set(1, 1, 2, 3), Queue(3, 3, 3, 3)) == Queue(1, 1, 1, 2, 2, 2, 3, 3, 3))
    assert(testFunction.duplicateWithNoReps(Set('a', 'n', 'n', 'a'), Queue(1, 2, 3, 4)) == Queue('a', 'n', 'n'))
  }

  @Test
  def testFunction3 = {
    val p: Point = new Point(3, 4)
    p.debugName

    val c: Car = new Car("Mercedes", "black")
    c.debugName

    val r: Rectangle = new Rectangle(5.5, 10.5)
    r.debugName
  }

  @Test
  def testFunction4 = {
    val p: Point = new Point(3, 4)
    p.debugVars()

    val c: Car = new Car("Mercedes", "black")
    c.debugVars()

    val r: Rectangle = new Rectangle(5.5, 10.5)
    r.debugVars()
  }

  @Test
  def testFunction5 = {
    //testing getName
    val p: Point = new Point(3, 4)
    assert(p.getName == "Point")

    val c: Car = new Car("Mercedes", "black")
    assert(c.getName == "Car")

    val r: Rectangle = new Rectangle(5.5, 10.5)
    assert(r.getName == "Rectangle")

    //testing getVars (by printing)
    printMap(p.getVars)
    printMap(c.getVars)
    printMap(r.getVars)
  }

  def printMap(m: Map[String, (Class[_], AnyRef)]) =
    m.foreach {
      case (key, (valType, value)) => println(s"Var $key => $valType, $value")
    }
}
