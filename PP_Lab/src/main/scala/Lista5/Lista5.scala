package Lista5

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.immutable.HashSet

object Lista5 {
  //zadanie 1 (2.5 pkt)
  //wg. dokumentacji: enqueue, dequeue, front - O(1)
  def duplicate[A](elementCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    @tailrec
    def innerDuplicate(elements: Queue[A], repetitions: Queue[Int], retCollection: Queue[A], currentCounter: Int): Queue[A] = {
      if (currentCounter > 0 )
        innerDuplicate(elements, repetitions, retCollection.enqueue(elements.front), currentCounter-1)
      else {
        val repetitionDequeue = repetitions.dequeue
        val elementsDequeue = elements.dequeue
        if (repetitionDequeue._2.isEmpty || elementsDequeue._2.isEmpty)
          retCollection
        else
          innerDuplicate(elementsDequeue._2, repetitionDequeue._2, retCollection, repetitionDequeue._2.front)
      }
    }

    if(elementCollection.isEmpty || repetitionsCollection.isEmpty)
      Queue()
    else
      innerDuplicate(elementCollection, repetitionsCollection, Queue(), repetitionsCollection.front)
  }


  //zadanie 2 (2.5 pkt)
  //wg. dokumentacji: enqueue, dequeue, front : O(1). Dla hashSet contains,+ : O(1)
  def duplicateWithoutRepeating[A](elementCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    if(elementCollection.isEmpty || repetitionsCollection.isEmpty)
      Queue()
    else {
      @tailrec
      //usuwanie powtorzen z kolejki elementow oraz odpowiadajace im wartosci powtorzen z kolejki ilosci powtorzen
      def innerPrepare(elements: Queue[A], repetitions: Queue[Int], newElements: Queue[A], newRepetition: Queue[Int], checkSet: HashSet[A]): (Queue[A], Queue[Int]) = {
        if (elements.isEmpty || repetitions.isEmpty)
          (newElements, newRepetition)
        else {
          if (checkSet.contains(elements.front))
            innerPrepare(elements.dequeue._2, repetitions.dequeue._2, newElements, newRepetition, checkSet)
          else {
            val repetitionDequeue = repetitions.dequeue
            val elementsDequeue = elements.dequeue
            innerPrepare(elementsDequeue._2, repetitionDequeue._2, newElements.enqueue(elementsDequeue._1), newRepetition.enqueue(repetitionDequeue._1), checkSet+elementsDequeue._1)
          }
        }
      }
      val processedQueues = innerPrepare(elementCollection, repetitionsCollection, Queue(), Queue(), HashSet())
      duplicate(processedQueues._1, processedQueues._2)
    }
  }

  //zadanie 3 (5 pkt)
  //zadanie 4 (5 pkt)
  //zadanie 5 (5 pkt)




}
