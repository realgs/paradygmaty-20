import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Collections {

  //Zad 1 (2,5 ptk)
  def duplicate[A](collectionOfElements: Queue[A], collectionOfRepetitionNumbers: Queue[Int]): Queue[A] = {
    @tailrec
    def tailDuplicate(elements: Queue[A], repetitions: Queue[Int], resultCollection: Queue[A], repCounter: Int): Queue[A] = {
      if (repCounter > 0 )
        tailDuplicate(elements, repetitions, resultCollection.enqueue(elements.front), repCounter-1)
      else {
        val (_, newElements) = elements.dequeue
        val (_, newRepetitions) = repetitions.dequeue
        if (newElements.isEmpty || newRepetitions.isEmpty)
          resultCollection
        else
          tailDuplicate(newElements, newRepetitions, resultCollection, newRepetitions.front)
      }
    }

    if(collectionOfElements.isEmpty || collectionOfRepetitionNumbers.isEmpty)
      Queue()
    else
      tailDuplicate(collectionOfElements, collectionOfRepetitionNumbers, Queue(), collectionOfRepetitionNumbers.front)
  }

  //Zad 2 (2,5 ptk)
  //Wszystkie powtarzające się elementy w kolekcji wejściowej są ignorowane, ale ma to wpływ na podane liczby powtórzeń dla konkretnych elementów
  def duplicateNoEntryDuplicates[A](collectionOfElements: Set[A], collectionOfRepetitionNumbers: Queue[Int]): Queue[A] = {
    @tailrec
    def tailDuplicateNoEntryDuplicates(elements: Set[A], repetitions: Queue[Int], resultCollection: Queue[A], repCounter: Int): Queue[A] = {
      if (repCounter > 0 )
        tailDuplicateNoEntryDuplicates(elements, repetitions, resultCollection.enqueue(elements.head), repCounter-1)
      else {
        val newElements = elements.tail
        val (_, newRepetitions) = repetitions.dequeue
        if (newElements.isEmpty || newRepetitions.isEmpty)
          resultCollection
        else
          tailDuplicateNoEntryDuplicates(newElements, newRepetitions, resultCollection, newRepetitions.front)
      }
    }

    if(collectionOfElements.isEmpty || collectionOfRepetitionNumbers.isEmpty)
      Queue()
    else
      tailDuplicateNoEntryDuplicates(collectionOfElements, collectionOfRepetitionNumbers, Queue(), collectionOfRepetitionNumbers.front)
  }
}
