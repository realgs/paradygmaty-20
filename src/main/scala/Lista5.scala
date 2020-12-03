import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object Lista5 extends App {

  // zadanie 1 (2,5 pkt)
  // na wejsciowych listach wykonujemy przede wszystkim operacje pobierania glowy i przekazywanie jej ogona dalej
  // metody head oraz tail sa wykonywane w czasie stalym, stad moj wybor tej struktury danych
  def duplicate[A](elementsToDuplicate: List[A], duplicateTimes: List[Int]): Queue[A] = {
    @tailrec
    def duplicateElement(element: A, howManyDuplicates: Int, resultQueue: Queue[A]): Queue[A] = {
      if(howManyDuplicates == 0) resultQueue
      else if(howManyDuplicates > 0) duplicateElement(element, howManyDuplicates - 1, resultQueue.enqueue(element))
      else throw new IllegalArgumentException("Negative number of duplicates is not allowed")
    }

    @tailrec
    def duplicateHelper(elementsToDuplicate: List[A], duplicateTimes: List[Int], resultQueue: Queue[A]): Queue[A] = {
      if(elementsToDuplicate == Nil || duplicateTimes == Nil) resultQueue
      else duplicateHelper(elementsToDuplicate.tail, duplicateTimes.tail, duplicateElement(elementsToDuplicate.head, duplicateTimes.head, resultQueue))
    }

    if(elementsToDuplicate == Nil || duplicateTimes == Nil) Queue()
    else duplicateHelper(elementsToDuplicate, duplicateTimes, Queue())
  }

  // zadanie 2 (2,5 pkt)
  // wykorzystuje strukture LinkedHashSet, poniewaz nie dopuszcza on duplikatow, a oprocz tego iterowanie po tej kolekcji
  // sprawia ze otrzymujemy elementy w takiej kolejnosci w jakiej wpisalismy je do listy
  def duplicateWithoutRepetitions[A](elementsWithoutRepetitions: mutable.LinkedHashSet[A], duplicateTimes: List[Int]) : Queue[A] = {
    if(elementsWithoutRepetitions.isEmpty || duplicateTimes == Nil) Queue()
    else duplicate(elementsWithoutRepetitions.toList, duplicateTimes)
  }

  trait Debug {
    // zadanie 3 (5 pkt)
    def debugName(): Unit = {
      println(s"Class: ${getClass.getSimpleName}")
    }

    // zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      for(field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println(s"Var: ${field.getName} => ${field.getAnnotatedType}, ${field.get(this)}")
      }
    }
  }
}
