import java.lang.reflect.AnnotatedElement

import Solutions.{Debug, duplicate, duplicateMutable, duplicateWithoutReps, duplicateWithoutRepsMutable}

import scala.collection.immutable.Queue

object Tests {
  def main(args: Array[String]): Unit = {
    // Task 1
    println(duplicate(Queue(), Queue(0, 3, 1, 4)))
    println(duplicate(Queue(1, 2, 3), Queue()))
    println(duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)))
    println(duplicate(Queue(1, 2, 3), Queue(-1, 1, 0)))
    println(duplicate(Queue(1, 2, 3), Queue(0, 4)))
    println(duplicate(Queue(4, 5, 6, 7, 8), Queue(1, 0, 1, 0, 1)))
    println(duplicate(Queue(1), Queue(0, 2)))
    println(duplicate(Queue(1), Queue(5)))
    println(duplicate(Queue("test", "te"), Queue(2, 0)))
    println(duplicate(Queue("test", "te"), Queue(3, 2)))

    import scala.collection.mutable

    println(duplicateMutable(mutable.Queue(), mutable.Queue(0, 3, 1, 4)))
    println(duplicateMutable(mutable.Queue(1, 2, 3), mutable.Queue()))
    println(duplicateMutable(mutable.Queue(1, 2, 3), mutable.Queue(0, 3, 1, 4)))
    println(duplicateMutable(mutable.Queue(1, 2, 3), mutable.Queue(-1, 1, 0)))
    println(duplicateMutable(mutable.Queue(1, 2, 3), mutable.Queue(0, 4)))
    println(duplicateMutable(mutable.Queue(4, 5, 6, 7, 8), mutable.Queue(1, 0, 1, 0, 1)))
    println(duplicateMutable(mutable.Queue(1), mutable.Queue(0, 2)))
    println(duplicateMutable(mutable.Queue(1), mutable.Queue(5)))
    println(duplicateMutable(mutable.Queue("test", "te"), mutable.Queue(2, 0)))
    println(duplicateMutable(mutable.Queue("test", "te"), mutable.Queue(3, 2)))

    // Task 2
    println(duplicateWithoutReps(Queue(), Queue()))
    println(duplicateWithoutReps(Queue(), Queue(1)))
    println(duplicateWithoutReps(Queue(2), Queue()))
    println(duplicateWithoutReps(Queue(1, 1, 1), Queue(2)))
    println(duplicateWithoutReps(Queue(2, 2, 3), Queue(1, 1, 0)))
    println(duplicateWithoutReps(Queue(4, 5, 4, 7, 4), Queue(1, 2, 1, 2, 1)))
    println(duplicateWithoutReps(Queue(9, 8, 7, 9), Queue(-1, -1, -1, 0)))
    println(duplicateWithoutReps(Queue(9, 8, 7, 9), Queue(2, 1, 0, 2)))

    println(duplicateWithoutRepsMutable(mutable.Queue(), mutable.Queue()))
    println(duplicateWithoutRepsMutable(mutable.Queue(), mutable.Queue(1)))
    println(duplicateWithoutRepsMutable(mutable.Queue(2), mutable.Queue()))
    println(duplicateWithoutRepsMutable(mutable.Queue(1, 1, 1), mutable.Queue(2)))
    println(duplicateWithoutRepsMutable(mutable.Queue(2, 2, 3), mutable.Queue(1, 1, 0)))
    println(duplicateWithoutRepsMutable(mutable.Queue(4, 5, 4, 7, 4), mutable.Queue(1, 2, 1, 2, 1)))
    println(duplicateWithoutRepsMutable(mutable.Queue(9, 8, 7, 9), mutable.Queue(-1, -1, -1, 0)))
    println(duplicateWithoutRepsMutable(mutable.Queue(9, 8, 7, 9), mutable.Queue(2, 1, 0, 2)))

    // Task 3, 4, 5
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
      def x_= (newValue: Int): Unit = {
        if (newValue > bounds._1 && newValue < bounds._2) _x = newValue else printWarning()
      }

      def y: Int = _y
      def y_= (newValue: Int): Unit = {
        if (newValue > bounds._1 && newValue < bounds._2) _y = newValue else printWarning()
      }

      private def printWarning(): Unit = println("WARNING: Out of bounds")
    }

    // Task 3 + 4 + 5
    def handleMapPrint(map: Map[String, (AnnotatedElement, AnyRef)]): Unit = {
      map.foreach { case (key, (valType, value)) => println(s"Var $key => $valType, $value - modified val: ${value + "_mod"}")}
    }

    val p : Point = new Point(3,4)
    p.debugName()
    p.debugVars()

    val className1 = p.getName
    println(className1 + " - accessible")

    val classFields1 = p.getFields

    println(classFields1)
    println(classFields1.apply("x"))
    println(classFields1.apply("y"))

    handleMapPrint(classFields1)

    val advPoint = new AdvPoint(1, 2, 0, 100)
    advPoint.debugName()
    advPoint.debugVars()

    val className2 = advPoint.getName
    println(className2)

    val classFields2 = advPoint.getFields

    println(classFields2)
    val bounds = classFields2.apply("bounds")
    println("Bounds: " + bounds._2)

    handleMapPrint(classFields2)
  }
}

