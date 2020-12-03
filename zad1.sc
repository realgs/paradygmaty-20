//zad1 (2,5pkt)

import scala.annotation.tailrec
import scala.collection.immutable.Queue

def duplicate[A](elements: Queue[A], repsForElems: Queue[Int]): Queue[A] = {
  @tailrec
  def duplicateHelp[A](elements: Queue[A], repsForElems: Queue[Int], repCounter: Int, outputQueue: Queue[A]): Queue[A] = {
    if (repCounter > 0) duplicateHelp(elements, repsForElems, repCounter - 1, outputQueue.enqueue(elements.front))
    else {
      val restOfElems = elements.dequeue._2
      val restOfReps = repsForElems.dequeue._2
      if (restOfReps.nonEmpty && restOfElems.nonEmpty) duplicateHelp(restOfElems,restOfReps,restOfReps.front,outputQueue)
      else outputQueue
    }
  }
  if (elements.isEmpty || repsForElems.isEmpty) Queue.empty
  else duplicateHelp(elements,repsForElems,repsForElems.front,Queue.empty)
}

duplicate(Queue(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3)
duplicate(Queue(2, 4, 6), Queue(1, 2, 3)) == Queue(2, 4, 4, 6, 6, 6)
duplicate(Queue(2), Queue()) == Queue()
duplicate(Queue(), Queue(2)) == Queue()
duplicate(Queue(2), Queue(0)) == Queue()
duplicate(Queue(2), Queue(-1)) == Queue()
duplicate(Queue('a', 'b', 'c'), Queue(0, 3)) == Queue('b', 'b', 'b')
duplicate(Queue("a"), Queue(3, -1)) == Queue("a", "a", "a")

