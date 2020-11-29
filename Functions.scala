import scala.collection.immutable.Queue

class Functions {
  //ex 1 (2.5 pkt)
  def duplicate[A](elements: Queue[A], repetitions: Queue[Int]): Queue[A] = {
    @scala.annotation.tailrec
    def duplicateInner[A](elements: Queue[A], element: A, repetitions: Queue[Int], counter: Int, resultQueue: Queue[A]): Queue[A] = {
      if (counter > 0)
        duplicateInner(elements, element, repetitions, counter - 1, resultQueue.enqueue(element))
      else if (elements.isEmpty || repetitions.isEmpty) {
        resultQueue
      }
      else {
        val (nextElement, nextElements) = elements.dequeue
        val (nextCounter, nextRepetitions) = repetitions.dequeue
        duplicateInner(nextElements, nextElement, nextRepetitions, nextCounter, resultQueue)
      }
    }

    if (elements.isEmpty || repetitions.isEmpty)
      Queue()
    else {
      val (nextElement, nextElements) = elements.dequeue
      val (nextCounter, nextRepetitions) = repetitions.dequeue
      duplicateInner(nextElements, nextElement, nextRepetitions, nextCounter, Queue())
    }
  }

  //ex 2 (2.5 pkt)
  def duplicateWithNoReps[A](elements: Set[A], repetitions: Queue[Int]): Queue[A] = {
    @scala.annotation.tailrec
    def duplicateInnerWithNoReps[A](elements: Set[A], element: A, repetitions: Queue[Int], counter: Int, resultQueue: Queue[A]): Queue[A] = {
      if (counter > 0)
        duplicateInnerWithNoReps(elements, element, repetitions, counter - 1, resultQueue.enqueue(element))
      else if (elements.isEmpty || repetitions.isEmpty) {
        resultQueue
      }
      else {
        val (nextCounter, nextRepetitions) = repetitions.dequeue
        duplicateInnerWithNoReps(elements.tail, elements.head, nextRepetitions, nextCounter, resultQueue)
      }
    }

    if (elements.isEmpty || repetitions.isEmpty)
      Queue()
    else {
      val (nextCounter, nextRepetitions) = repetitions.dequeue
      duplicateInnerWithNoReps(elements.tail, elements.head, nextRepetitions, nextCounter, Queue())
    }
  }




}
