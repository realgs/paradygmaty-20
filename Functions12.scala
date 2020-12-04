import scala.collection.mutable

object Functions12 {
  // Zadanie 1 2.5pkt
  def duplicate [A](toReplicate: mutable.Queue[A], factors: List[Int]): mutable.Queue[A] =
    {
      val output = mutable.Queue[A]()
      for (i <- factors if toReplicate.nonEmpty)
        {
          val elem = toReplicate.dequeue
          for (_ <- 0 until i)
            output.enqueue(elem)
        }
      if (toReplicate.nonEmpty)
        output.appendAll(toReplicate)
      return output
    }

  // Zadanie 2 2.5pkt
  def duplicateMod [A](toReplicate: mutable.Queue[A], factors: List[Int]): (mutable.Queue[A], Boolean) =
    {
      if (toReplicate.isEmpty)
        return (mutable.Queue(), false)
      if (factors == Nil)
        return (toReplicate, false)

      @scala.annotation.tailrec
      def duplicate(factors: List[Int], set: Set[A], output: mutable.Queue[A]): Unit =
        {
          if(factors != Nil && set.nonEmpty)
            {
              for (_ <- 0 until factors.head)
                output.enqueue(set.head)
              duplicate(factors.tail, set.tail, output)
            }
          else if(factors == Nil && set.nonEmpty)
            {
              output.enqueueAll(set)
            }
        }
      val map = mutable.HashMap[A, Int]()
      val output = mutable.Queue[A]()
      var wasDuplicate = false
      for (elem <- toReplicate if !wasDuplicate)
        {
          if (map.put(elem, 0).isDefined)
            {
              println("Queue contained duplicates. Removed them and performed replication.")
              wasDuplicate = true
            }
        }
      val set = toReplicate.toSet
      duplicate(factors, set, output)
      return (output, wasDuplicate)
    }
}
