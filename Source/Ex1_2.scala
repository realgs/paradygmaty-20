package Source


import scala.collection.{mutable => m}
import scala.math.max


object Ex1_2 {
  // Helper for ex 1 and 2
  private def putRepetitionsInQueue[A](queue: m.Queue[A], element: A, n_repeats: Int): Unit =
  {
    for {_ <- 0 until n_repeats}
    {
      queue.enqueue(element)
    }
  }
  // Zadanie 1 (2.5 pkt)
  // =======================================================
  // Assumptions:
  // - size of numRepeats >= size of collection
  // - negative number in numRepeats means 0
  // =======================================================
  // Chosen input structure: Queue
  // - get element <- O(1)
  // - is iterable
  // - elements are kept in order
  // Chosen output structure: Queue
  // - append element <- O(1)
  def duplicate[A](collection: m.Queue[A], numRepeats: m.Queue[Int]): m.Queue[A] =
  {
    if (collection.size > numRepeats.size) throw new Exception("NumRepeats size must be greater or equal to the size of the collection")
    val resultQueue = m.Queue[A]()
    while (collection.nonEmpty)
    {
      val element = collection.dequeue()
      val repeats = max(numRepeats.dequeue(), 0)
      putRepetitionsInQueue(resultQueue, element, repeats)
    }
    resultQueue
  }

  // Zadanie 2 (2.5 pkt)
  // =======================================================
  // Assumptions:
  // - size of numRepeats >= size of collection
  // - negative number in numRepeats means 0
  // =======================================================
  // Chosen input structure: LinkedHashSet
  // - get element <- O(1) (using iterator)
  // - elements cannot be duplicated
  // - is iterable
  // - elements are kept in order
  // Chosen output structure: Queue
  // - append element <- O(1)
  def duplicateNoRepetitions[A](collection: m.LinkedHashSet[A], numRepeats: m.Queue[Int]): m.Queue[A] =
  {
    if (collection.size > numRepeats.size) throw new Exception("NumRepeats size must be greater or equal to the size of the collection")
    val resultQueue = m.Queue[A]()
    val iterator = collection.iterator
    while (iterator.hasNext)
    {
      val element = iterator.next()
      val repeats = max(numRepeats.dequeue(), 0)
      putRepetitionsInQueue(resultQueue, element, repeats)
    }
    resultQueue
  }
}
