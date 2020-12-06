import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Functions {
  // Zad 1 (2.5 pkt)
  def duplicate[A](collection: Seq[A])(reps: Seq[Int]): Seq[A] = {
    val padded = reps.padTo(collection.length, 1)
    collection.lazyZip(padded).flatMap((elem, count) => Queue.fill(count)(elem)) // Note that Queue has O(1) prepend
  }

  // Zad 2 (2.5 pkt)
  def duplicateDistinct[A](collection: Seq[A])(mask: Seq[Int]): Seq[A] = {
    println("Taking only distinct elements of passed collection!")
    duplicate(collection.distinct)(mask)
  }

  // Zad 1/2 without using library functions
  def formQueue[A](count: Int)(elem: A): Queue[A] = {
    @tailrec
    def auxFormQueue(count: Int)(accu: Queue[A]): Queue[A] = count match {
      case x if x > 0 => auxFormQueue(x - 1)(accu.enqueue(elem))
      case _ => accu
    }

    auxFormQueue(count)(Queue())
  }

  def duplicateVerbose[A](collection: Seq[A])(reps: Seq[Int]): Queue[A] = {
    val colIter = collection.iterator
    val repsIter = reps.iterator
    var accu: Queue[A] = Queue()

    while (colIter.hasNext & repsIter.hasNext) {
      accu = accu.enqueueAll(formQueue(repsIter.next())(colIter.next()))
    }

    if (colIter.hasNext) accu = accu ++ colIter

    accu
  }

  trait Debug {
    def getDebugName: String = getClass.getSimpleName

    // Zad 4 (5 pkt)
    def debugName(): Unit = printf("Class: %s\n", getDebugName)

    // Zad 5 (5 pkt)
    def getDebugVarsMap: Map[String, Array[String]] = {
      // Drop $outer and tapEach with setAccessible
      val fields = getClass.getDeclaredFields.filter(_.getName != "$outer").tapEach(_.setAccessible(true))

      // Create Map[String, Array[String]] : identifier -> Array(value, typename)
      val nameTypeMap = fields.map { f => f.getName -> Array(f.get(this).toString, f.getType.getSimpleName) }.toMap

      nameTypeMap
    }

    def debugVars(): Unit = {
      getDebugVarsMap.foreachEntry((ident, info) => printf("var %s (%s) = %s\n", ident, info(1), info(0)))
    }
  }

}
