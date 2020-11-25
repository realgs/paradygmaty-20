import java.lang.reflect.AnnotatedElement

import Solutions.{Debug, duplicate, duplicateWithoutReps}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Queue

class Tests extends AnyFunSuite {
  test("task 1") {
    assert(duplicate(Queue(), Queue(0, 3, 1, 4)) == Queue())
    assert(duplicate(Queue(), Queue(0, 3, 1, 4)) == Queue())
    assert(duplicate(Queue(1, 2, 3), Queue()) == Queue())
    assert(duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3))
    assert(duplicate(Queue(1, 2, 3), Queue(-1, 1, 0)) == Queue(2))
    assert(duplicate(Queue(1, 2, 3), Queue(0, 4)) == Queue(2, 2, 2, 2))
    assert(duplicate(Queue(4, 5, 6, 7, 8), Queue(1, 0, 1, 0, 1)) == Queue(4, 6, 8))
    assert(duplicate(Queue(1), Queue(0, 2)) == Queue())
    assert(duplicate(Queue(1), Queue(5)) == Queue(1, 1, 1, 1, 1))
    assert(duplicate(Queue("test", "te"), Queue(2, 0)) == Queue("test", "test"))
    assert(duplicate(Queue("test", "te"), Queue(3, 2)) == Queue("test", "test", "test", "te", "te"))
  }

  test("task 2") {
    assert(duplicateWithoutReps(Set(), Queue()) == Queue())
    assert(duplicateWithoutReps(Set(), Queue(1)) == Queue())
    assert(duplicateWithoutReps(Set(2), Queue()) == Queue())
    assert(duplicateWithoutReps(Set(1, 1, 1), Queue(2)) == Queue(1, 1))
    assert(duplicateWithoutReps(Set(2, 2, 3), Queue(1, 1, 0)) == Queue(2, 3))
    assert(duplicateWithoutReps(Set(4, 5, 4, 7, 4), Queue(1, 2, 1, 2, 1)) == Queue(4, 5, 5, 7))
    assert(duplicateWithoutReps(Set(9, 8, 7, 9), Queue(-1, -1, -1, 0)) == Queue())
    assert(duplicateWithoutReps(Set(9, 8, 7, 9), Queue(2, -1, 0, 3)) == Queue(9, 9))
    assert(duplicateWithoutReps(Set(9, 8, 7, 9), Queue(2, 1, 0, 2)) == Queue(9, 9, 8))
    assert(duplicateWithoutReps(Set(1, 1, 2, 4, 4), Queue(1, 2, 1, 0)) == Queue(1, 2, 2, 4))
  }

  test("tasks 3 + 4 + 5") {
    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

    class AdvPoint(xv: Int, yv: Int, minBound: Int, maxBound: Int) extends Debug {
      private var _x = xv
      private var _y = yv
      private val bounds = (minBound, maxBound)

      def x: Int = _x

      def x_=(newValue: Int): Unit = {
        if (newValue > bounds._1 && newValue < bounds._2) _x = newValue else printWarning()
      }

      def y: Int = _y

      def y_=(newValue: Int): Unit = {
        if (newValue > bounds._1 && newValue < bounds._2) _y = newValue else printWarning()
      }

      private def printWarning(): Unit = println("WARNING: Out of bounds")
    }

    def handleMapPrint(map: Map[String, (AnnotatedElement, AnyRef)]): Unit = {
      map.foreach { case (key, (valType, value)) => println(s"Var $key => $valType, $value - modified val: ${value + "_mod"}") }
    }

    val p: Point = new Point(3, 4)
    p.debugName()
    p.debugVars()

    val className1 = p.getName
    assert(className1 + " - accessible" == "Point$1 - accessible")

    val classFields1 = p.getFields
    println(classFields1)

    val x = classFields1.apply("x")
    val y = classFields1.apply("y")
    assert(x._2 == 3)
    assert(y._2 == 4)

    handleMapPrint(classFields1)

    val advPoint = new AdvPoint(1, 2, 0, 100)
    advPoint.debugName()
    advPoint.debugVars()

    val className2 = advPoint.getName
    assert(className2 + " - accessible" == "AdvPoint$1 - accessible")

    val classFields2 = advPoint.getFields
    println(classFields2)

    val bounds = classFields2.apply("bounds")
    assert(bounds._2 == (0,100))

    handleMapPrint(classFields2)
  }
}
