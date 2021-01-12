package scala

import scala.annotation.tailrec
import scala.collection.immutable.{ListSet, Queue}


object ExerciseOneAndTwo {
  // zad1 (2.5pkt)
  // w zadaniu tym wybralem kolejke, gdyz umozliwia ona dodawanie elementu na koniec w czasie stalym
  // mozna by oczywiscie uzyc listy i tworzyc liste wynikowa korzystajac z rekurencji, jednak niemozliwe byloby wtedy uzycie rekursji ogonowej
  def duplicate[A](collection: Queue[A], repsCollection: Queue[Int]): Queue[A] = {
    @tailrec
    def innerDuplicate(coll: Queue[A], repsColl: Queue[Int], result: Queue[A], counter: Int): Queue[A] = {
      val (elemToRepeat, collTail) = coll.dequeue

      if (counter > 0) innerDuplicate(coll, repsColl, result.enqueue(elemToRepeat), counter - 1)
      else if (counter == 0) {
        if (collTail != Queue()) innerDuplicate(collTail, repsColl.tail, result, repsColl.head)
        else result
      }
      else throw new IllegalArgumentException("Element can't be repetead negative number of times")
    }

    if (collection.length > repsCollection.length) throw new IllegalArgumentException("first collection length can't be less than repsCollection")
    else innerDuplicate(collection, repsCollection.tail, Queue.empty, repsCollection.head)
  }

  // zad2 (2.5pkt)
  // w wejsciowej kolekcji zamiast kolejki uzylem kolekcji ListSet
  // zdecydowalem sie uzyc kolekcji ListSet jako wejsciowej kolekcji, gdyz respektuje ona kolejnosc w jakiej elementy byly dodawane, a takze ignoruje duplikaty
  def duplicateWithoutDuplicates[A](collection: ListSet[A], repsCollection: Queue[Int]): Queue[A] = {
    @tailrec
    def innerDuplicate(coll: ListSet[A], repsColl: Queue[Int], result: Queue[A], counter: Int): Queue[A] = {
      val (elemToRepeat, collTail) = (coll.head, coll.tail)

      if (counter > 0) innerDuplicate(coll, repsColl, result.enqueue(elemToRepeat), counter - 1)
      else if (counter == 0) {
        if (collTail != ListSet()) innerDuplicate(collTail, repsColl.tail, result, repsColl.head)
        else result
      }
      else throw new IllegalArgumentException("Element can't be repeated negative number of times")
    }

    if (collection.size > repsCollection.length) throw new IllegalArgumentException("first collection length can't be less than repsCollection")
    else innerDuplicate(collection, repsCollection.tail, Queue.empty, repsCollection.head)
  }
}
