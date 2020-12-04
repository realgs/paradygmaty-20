package lab05

import scala.collection.immutable.Queue

object Functions {

  //zadanie 1 (2,5pkt)
  def duplicate[A](elementsCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    @scala.annotation.tailrec
    def duplicateElements(element: A, elementQueue: Queue[A], repetition: Int, repetitionQueue: Queue[Int], result: Queue[A]): Queue[A] = {
      repetition match {
        case 0 =>
          if (elementQueue.isEmpty || repetitionQueue.isEmpty) result
          else {
            val elemTuple = elementQueue.dequeue
            val repsTuple = repetitionQueue.dequeue
            duplicateElements(elemTuple._1, elemTuple._2, repsTuple._1, repsTuple._2, result)
          }
        case _ => duplicateElements(element, elementQueue, repetition - 1, repetitionQueue, result.enqueue(element))
      }
    }

    (elementsCollection, repetitionsCollection) match {
      case (Queue(), _) => Queue()
      case (_, Queue()) => Queue()
      case (_, _) =>
        val elementsTuple = elementsCollection.dequeue
        val repetitionsTuple = repetitionsCollection.dequeue
        duplicateElements(elementsTuple._1, elementsTuple._2, repetitionsTuple._1, repetitionsTuple._2, Queue())
    }
  }

  //zadanie 2 (2,5pkt)
  def duplicateWithoutRepetitions[A](elementsCollection: Set[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    @scala.annotation.tailrec
    def duplicateElements(element: A, elementsSet: Set[A], repetition: Int, repetitionQueue: Queue[Int], result: Queue[A]): Queue[A] = {
      repetition match {
        case 0 =>
          if (elementsSet.isEmpty || repetitionQueue.isEmpty) result
          else {
            val repsTuple = repetitionQueue.dequeue
            duplicateElements(elementsSet.head, elementsSet.tail, repsTuple._1, repsTuple._2, result)
          }
        case _ => duplicateElements(element, elementsSet, repetition - 1, repetitionQueue, result.enqueue(element))
      }
    }

    if (elementsCollection.isEmpty || repetitionsCollection.isEmpty) Queue()
    else {
      val repetitionsTuple = repetitionsCollection.dequeue
      duplicateElements(elementsCollection.head, elementsCollection.tail, repetitionsTuple._1, repetitionsTuple._2, Queue())
    }
  }

}

