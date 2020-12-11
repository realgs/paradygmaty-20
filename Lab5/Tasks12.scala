import scala.collection.mutable

object Tasks12 {
  // Zadanie 1 (2.5pkt)
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
