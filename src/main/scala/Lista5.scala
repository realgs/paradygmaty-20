import java.lang.reflect.{AnnotatedElement, Field}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.LinkedHashSet

object Lista5 extends App {

  // zadanie 1 (2,5 pkt)
  def duplicate[A](elementsToDuplicate: Queue[A], duplicateTimes: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateElement(element: A, howManyDuplicates: Int, resultQueue: Queue[A]): Queue[A] = {
      if (howManyDuplicates == 0) resultQueue
      else if (howManyDuplicates > 0) duplicateElement(element, howManyDuplicates - 1, resultQueue.enqueue(element))
      else throw new IllegalArgumentException("Negative number of duplicates is not allowed")
    }

    @tailrec
    def duplicateHelper(elementsToDuplicate: Queue[A], duplicateTimes: Queue[Int], resultQueue: Queue[A]): Queue[A] = {
      if (elementsToDuplicate.isEmpty || duplicateTimes.isEmpty) resultQueue
      else duplicateHelper(elementsToDuplicate.dequeue._2, duplicateTimes.dequeue._2, duplicateElement(elementsToDuplicate.front, duplicateTimes.front, resultQueue))
    }

    if (elementsToDuplicate.isEmpty || duplicateTimes.isEmpty) Queue.empty
    else duplicateHelper(elementsToDuplicate, duplicateTimes, Queue.empty)
  }

  // zadanie 2 (2,5 pkt)
  // wykorzystuje strukture LinkedHashSet, poniewaz nie dopuszcza on duplikatow, a oprocz tego iterowanie po tej kolekcji
  // sprawia ze otrzymujemy elementy w takiej kolejnosci w jakiej wpisalismy je do listy
  def duplicateWithoutRepetitions[A](elementsWithoutRepetitions: LinkedHashSet[A], duplicateTimes: Queue[Int]): Queue[A] = {
    if (elementsWithoutRepetitions.isEmpty || duplicateTimes.isEmpty) Queue.empty
    else {
      var queue = Queue[A]()
      for(el <- elementsWithoutRepetitions){
        queue = queue.enqueue(el)
      }

      duplicate(queue, duplicateTimes)
    }
  }

  trait Debug {
    // zadanie 3 (5 pkt)
    def debugName(): Unit = {
      println(s"Class: ${getClass.getSimpleName}")
    }

    // zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println(s"Var: ${field.getName} => ${field.getAnnotatedType}, ${field.get(this)}")
      }
    }

    // zadanie 5 (5 pkt)
    def getClassName: String = getClass.getSimpleName

    def getClassFields: List[(String, String, Any)] = {
      def getClassFieldsHelper(classFields: IndexedSeq[Field]): List[(String, String, Any)] = {
        if (classFields.isEmpty) Nil
        else {
          val field = classFields.head
          field.setAccessible(true)
          (field.getName, field.getAnnotatedType.toString, field.get(this)) :: getClassFieldsHelper(classFields.tail)
        }
      }

      getClassFieldsHelper(getClass.getDeclaredFields)
    }
  }
}
