import java.lang.reflect.AnnotatedElement

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Tasks {

  // Task 1. (2.5 pkt)
  def duplicate[A](collection: Queue[A], numbersOfDuplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateInner[B](collection: Queue[B], numbersOfDuplications: Queue[Int], elementToDuplicate: B, duplicationCounter: Int, result: Queue[B]): Queue[B] =
      // don't throw Exception if negative number
      if(duplicationCounter > 0) duplicateInner(collection, numbersOfDuplications, elementToDuplicate, duplicationCounter - 1, result.enqueue(elementToDuplicate))
      else if(collection.isEmpty || numbersOfDuplications.isEmpty) result
      else {
        val (first, newCollection) = collection.dequeue
        val (firstDuplicationNumber, newNumbersOfDuplications) = numbersOfDuplications.dequeue

        duplicateInner(newCollection, newNumbersOfDuplications, first, firstDuplicationNumber, result)
      }

    if(collection.isEmpty || numbersOfDuplications.isEmpty) Queue()
    else {
      val (first, newCollection) = collection.dequeue
      val (firstDuplicationNumber, newNumbersOfDuplications) = numbersOfDuplications.dequeue

      duplicateInner(newCollection, newNumbersOfDuplications, first, firstDuplicationNumber, Queue())
    }
  }

  // Task 2. (2.5 pkt)
  def duplicateWithoutRepetitions[A](collection: Set[A], numbersOfDuplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateInner[B](collection: Set[B], numbersOfDuplications: Queue[Int], elementToDuplicate: B, duplicationCounter: Int, result: Queue[B]): Queue[B] =
    // don't throw Exception if negative number
      if(duplicationCounter > 0) duplicateInner(collection, numbersOfDuplications, elementToDuplicate, duplicationCounter - 1, result.enqueue(elementToDuplicate))
      else if(collection.isEmpty || numbersOfDuplications.isEmpty) result
      else {
        val (firstDuplicationNumber, newNumbersOfDuplications) = numbersOfDuplications.dequeue

        duplicateInner(collection.tail, newNumbersOfDuplications, collection.head, firstDuplicationNumber, result)
      }

    if(collection.isEmpty || numbersOfDuplications.isEmpty) Queue()
    else {

      val (firstDuplicationNumber, newNumbersOfDuplications) = numbersOfDuplications.dequeue

      duplicateInner(collection.tail, newNumbersOfDuplications, collection.head, firstDuplicationNumber, Queue())
    }
  }

  // Tasks 3, 4, 5
  trait Debug {

    // Task 3. (5 pkt)
    def debugName(): Unit =
      println(s"Class: ${getClass.getSimpleName}")

    // Task 4. (5 pkt)
    def debugVars(): Unit =
      getClass.getDeclaredFields.foreach(field => {
        field.setAccessible(true)
        println(s"Var: ${field.getName} => ${field.getAnnotatedType}, ${field.get(this)}")
      })

    // Task 5. (5 pkt)
    def getClassName: String = getClass.getSimpleName

    def getClassFields: Map[String, (Class[_], AnyRef)] = {

      var fields: Map[String, (Class[_], AnyRef)] = Map()

      getClass.getDeclaredFields.foreach(field => {
        field.setAccessible(true)
        fields = fields + (field.getName -> (field.getType, field.get(this)))
      })

      fields
    }
  }
}
