import java.lang.reflect.AnnotatedElement
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.LinkedHashSet

object L5Lab extends App {
  // Zadanie 1 (2.5 pkt)
  def duplicate[A](elements: Queue[A], repetitions: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateHelper(elements: Queue[A], repetitions: Queue[Int], remainingRepetitions: Int, currentResult: Queue[A]): Queue[A] =
      if (remainingRepetitions > 0) {
        duplicateHelper(elements, repetitions, remainingRepetitions - 1, currentResult.enqueue(elements.front))
      } else if (elements.dequeue._2.nonEmpty && repetitions.dequeue._2.nonEmpty) {
        duplicateHelper(elements.dequeue._2, repetitions.dequeue._2, repetitions.dequeue._2.front, currentResult)
      } else {
        currentResult
      }

    if (repetitions.isEmpty || elements.isEmpty) {
      Queue.empty
    } else {
      duplicateHelper(elements, repetitions, repetitions.front, Queue.empty)
    }
  }

  // Zadanie 2 (2.5 pkt)
  def duplicateIgnoreDuplicates[A](elements: LinkedHashSet[A], repetitions: Queue[Int]): Queue[A] = {
    if (repetitions.isEmpty || elements.isEmpty) {
      Queue.empty
    } else {
      duplicate(Queue[A]() enqueueAll elements, repetitions)
    }
  }

  trait Debug {
    // Zadanie 3 (5 pkt)
    def printDebugName(): Unit = {
      println(s"Class: ${getClass.getSimpleName}")
    }

    // Zadanie 4 (5 pkt)
    def printDebugVariables(): Unit = {
      getClass.getDeclaredFields.foreach(field => {
        field.setAccessible(true)
        println(s"Var: ${field.getName} => ${field.getAnnotatedType}, ${field.get(this)}")
      })
    }

    // Zadanie 5 (5 pkt)
    def debugName: String = getClass.getSimpleName

    def debugVariables: Array[(String, AnnotatedElement, Any)] = {
      val fields = getClass.getDeclaredFields
      val debugVariables = new Array[(String, AnnotatedElement, Any)](fields.length)
      var i = 0
      fields.foreach(field => {
        field.setAccessible(true)
        debugVariables(i) = (field.getName, field.getAnnotatedType, field.get(this))
        i += 1
      })

      debugVariables
    }

  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Empty extends Debug

}
