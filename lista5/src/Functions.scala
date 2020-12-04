import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Functions {
  def duplicate[A](toDuplicate: Queue[A], duplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateIter(toDuplicate: Queue[A], duplications: Queue[Int], returnQueue: Queue[A], howMany: Int, element: A): Queue[A] = {
      if (howMany > 0) duplicateIter(toDuplicate, duplications, returnQueue.enqueue(element), howMany - 1, element)
      else if (toDuplicate.isEmpty || duplications.isEmpty) returnQueue
      else {
        val (newElement, tail1) = toDuplicate.dequeue
        val (newHowMany, tail2) = duplications.dequeue
        duplicateIter(tail1, tail2, returnQueue, newHowMany, newElement)
      }
    }

    val (element, tail1) = toDuplicate.dequeue
    val (howMany, tail2) = duplications.dequeue
    duplicateIter(tail1, tail2, Queue(), howMany, element)
  }

  def duplicateWithoutRepetition[A](toDuplicate: Set[A], duplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateWithoutRepetitionIter(toDuplicate: Set[A], duplications: Queue[Int], returnQueue: Queue[A], howMany: Int, element: A): Queue[A] = {
      if (howMany > 0) duplicateWithoutRepetitionIter(toDuplicate, duplications, returnQueue.enqueue(element), howMany - 1, element)
      else if (toDuplicate.isEmpty || duplications.isEmpty) returnQueue
      else {
        val (newHowMany, tail2) = duplications.dequeue
        duplicateWithoutRepetitionIter(toDuplicate.tail, tail2, returnQueue, newHowMany, toDuplicate.head)
      }
    }

    val (howMany, tail2) = duplications.dequeue
    duplicateWithoutRepetitionIter(toDuplicate.tail, tail2, Queue(), howMany, toDuplicate.head)
  }
}
