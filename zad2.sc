//zad2 (2,5pkt)

import scala.annotation.tailrec
import scala.collection.immutable.Queue

//Kolekcja set zapewnia brak duplikatów
def duplicate[A](elements: Set[A], repsForElems: Queue[Int]): Queue[A] = {
  @tailrec
  def duplicateHelp[A](elements: Set[A], repsForElems: Queue[Int], repCounter: Int, outputQueue: Queue[A]): Queue[A] = {
    if (repCounter > 0) duplicateHelp(elements, repsForElems, repCounter - 1, outputQueue.enqueue(elements.head))
    else {
      val restOfElems = elements.tail
      val restOfReps = repsForElems.dequeue._2
      if (restOfReps.nonEmpty && restOfElems.nonEmpty) duplicateHelp(restOfElems,restOfReps,restOfReps.front,outputQueue)
      else outputQueue
    }
  }
  if (elements.isEmpty || repsForElems.isEmpty) Queue.empty
  else duplicateHelp(elements,repsForElems,repsForElems.front,Queue.empty)
}

duplicate(Set(2), Queue(2)) == Queue(2, 2)
duplicate(Set(2), Queue(2,2)) == Queue(2, 2)
duplicate(Set(2,2), Queue(2,2)) == Queue(2, 2)
duplicate(Set(2,2,2,2,2,2,2), Queue(2,2)) == Queue(2, 2)
duplicate(Set(2, 2, 2), Queue(2, 2, 2, 4)) == Queue(2, 2)
duplicate(Set(1, 2, 3), Queue(0, 3, 1, 4)) == Queue(2, 2, 2, 3)
duplicate(Set(2, 4, 6), Queue(1, 2, 3)) == Queue(2, 4, 4, 6, 6, 6)
duplicate(Set(2), Queue()) == Queue()
duplicate(Set(), Queue(2)) == Queue()
duplicate(Set(2), Queue(0)) == Queue()
duplicate(Set(2), Queue(-1)) == Queue()
duplicate(Set('a', 'b', 'c'), Queue(0, 3)) == Queue('b', 'b', 'b')
duplicate(Set("a"), Queue(3, -1)) == Queue("a", "a", "a")

