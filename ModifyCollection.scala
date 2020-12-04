package L5
import scala.collection.immutable.{Queue, ListSet}
import scala.annotation.tailrec

object ModifyCollection {
  // helper method used in task 1 and 2
  @tailrec
  def addNTimes [A](queue: Queue[A], element: A, n: Int): Queue[A] = {
    if (n < 0) throw new Exception ("number of repeats must be positive")
    else if (n == 0) queue
    else addNTimes (queue.enqueue(element), element, n - 1)
  }

  // zadanie 1 (2.5 pkt)
  // pobierane są dwie listy, ponieważ zapewniają stałoczasowy dostęp do pierwszego elementu
  // zwracana jest kolejka, ponieważ zapewnia stałoczasowe dodawanie elementu na koniec
  def duplicateElements [A](collection: List[A], howManyRepeatsList: List[Int]): Queue[A] = {
    @tailrec
    def innerRepeat [A](collection: List[A], howManyRepeatsList: List[Int], resultQueue: Queue[A]): Queue[A] =
      (collection, howManyRepeatsList) match {
        case (head1 :: tail1, head2 :: tail2) => innerRepeat(tail1, tail2, addNTimes(resultQueue, head1, head2))
        case (_, Nil) => resultQueue
        case (Nil, _) => resultQueue
      }
    innerRepeat(collection, howManyRepeatsList, Queue())
  }

  // zadanie 2 (2.5 pkt)
  // pobierana jest ListSet, ponieważ zapewnia unikalność i zachowuje kolejność elementów
  // zwracana jest kolejka, ponieważ zapewnia stałoczasowe dodawanie elementu na koniec
  def duplicateElements [A](collection: ListSet[A], howManyRepeatsList: List[Int]): Queue[A] = {
    @tailrec
    def innerRepeat [A](collection: ListSet[A], howManyRepeatsList: List[Int], resultQueue: Queue[A]): Queue[A] =
      (collection, howManyRepeatsList) match {
        case (collection, head2 :: tail2) =>
          if (collection.isEmpty) resultQueue
          else innerRepeat(collection.tail, tail2, addNTimes(resultQueue, collection.head, head2))
        case (_, Nil) => resultQueue
      }
    innerRepeat(collection, howManyRepeatsList, Queue())
  }
}

