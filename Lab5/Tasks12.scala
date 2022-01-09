import scala.collection.mutable

object Tasks12 {
  // Zadanie 1 (2.5pkt)
  // chosen queue for "toDuplicate" 'cause dequeue and nonEmpty are O(1)
  // chosen list for "times" 'cause it's iterable
  // chosen queue for function output 'cause enqueue is O(1)
  def duplicate[A] (toDuplicate: mutable.Queue[A], times: List[Int]): mutable.Queue[A] =
  {
    val duplicated = mutable.Queue[A]()
    for (i <- times if toDuplicate.nonEmpty)
    {
      val x = toDuplicate.dequeue
      for (_ <- 0 until i)
        duplicated.enqueue(x)
    }
    duplicated
  }

  // Zadanie 2 (2.5pkt)
  // chosen linkedHashSet for "toDuplicate" 'cause it's iterable and elements are distinct
  // chosen queue for "times" 'cause dequeue and nonEmpty are O(1)
  // chosen queue for function output 'cause enqueue is O(1)
  def duplicateDistinct[A](toDuplicate: mutable.LinkedHashSet[A], times: mutable.Queue[Int]): mutable.Queue[A] =
  {
    val duplicated = mutable.Queue[A]()
    for (i <- toDuplicate if times.nonEmpty)
    {
      val n = times.dequeue
      for (_ <- 0 until n)
        duplicated.enqueue(i)
    }
    duplicated
  }
}
