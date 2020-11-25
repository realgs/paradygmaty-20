

import scala.annotation.tailrec
import scala.collection.mutable

object Lista5 {
  // zad 1
  // 2.5pkt
  def duplicate[A](entryCollection: mutable.ArrayDeque[A], duplicateCount: List[Int]): mutable.ArrayDeque[A] = {
    @tailrec
    def duplicateBody(entryCollection: mutable.ArrayDeque[A], accumulator: mutable.ArrayDeque[A], duplicateCount: List[Int]): mutable.ArrayDeque[A] =
      if (entryCollection.isEmpty) accumulator else {
        duplicateCount match {
          case Nil => accumulator
          case head :: tail => if (head <= 0) duplicateBody(entryCollection.tail, accumulator, tail)
          else duplicateBody(entryCollection, accumulator.append(entryCollection.head), (head - 1) +: tail)
        }
      }

    duplicateBody(entryCollection, new mutable.ArrayDeque[A](), duplicateCount)
  }

  // zad 2
  // 2.5 pkt
  @tailrec
  def duplicatesInArrayDeque[A](entryCollection: mutable.ArrayDeque[A]): Boolean = {
    // return true if duplicates exist in ArrayDeque, false otherwise
    @tailrec
    def duplInArrD(elem: A, kolekcja: mutable.ArrayDeque[A]): Boolean = {
      if (kolekcja.isEmpty)
        false
      else if (elem == kolekcja.head)
        true
      else
        duplInArrD(elem, kolekcja.tail)
    }

    if (entryCollection.isEmpty)
      false
    else
      duplInArrD(entryCollection.head, entryCollection.tail) || duplicatesInArrayDeque(entryCollection.tail)
  }

  def duplicate2[A](entryCollection: mutable.ArrayDeque[A], duplicateCount: List[Int]): mutable.ArrayDeque[A] = {
    @tailrec
    def duplicateBody(entryCollection: mutable.ArrayDeque[A], accumulator: mutable.ArrayDeque[A], duplicateCount: List[Int]): mutable.ArrayDeque[A] =
      if (entryCollection.isEmpty) accumulator else {
        duplicateCount match {
          case Nil => accumulator
          case head :: tail => if (head <= 0) duplicateBody(entryCollection.tail, accumulator, tail)
          else duplicateBody(entryCollection, accumulator.append(entryCollection.head), (head - 1) +: tail)
        }
      }

    if (duplicatesInArrayDeque(entryCollection))
      throw new Exception("Duplicates in entry collection!")
    else
      duplicateBody(entryCollection, new mutable.ArrayDeque[A](), duplicateCount)
  }

}
