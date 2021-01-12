import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Functions_L5 {
  //task1 (2.5p)
  def duplicate[A](toDuplicate: Queue[A], duplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateIter(toDuplicate: Queue[A], duplications: Queue[Int], returnQueue: Queue[A], howMany: Int, element: A): Queue[A] = {
      if (howMany > 0) duplicateIter(toDuplicate, duplications, returnQueue.enqueue(element), howMany - 1, element)
      else if (toDuplicate.isEmpty || duplications.isEmpty) returnQueue
      else {
        val (newElement, tail1) = toDuplicate.dequeue
        val (newHowMany, tail2) = duplications.dequeue
        duplicateIter(tail1, tail2, returnQueue, newHowMany, newElement)
      }
    }
    if (toDuplicate.isEmpty || duplications.isEmpty) Queue()
    else {
      val (element, tail1) = toDuplicate.dequeue
      val (howMany, tail2) = duplications.dequeue
      duplicateIter(tail1, tail2, Queue(), howMany, element)
    }
  }

  //task2 (2.5p)
  def duplicateWithoutRepetition[A](toDuplicate: Set[A], duplications: Queue[Int]): Queue[A] = {
    @tailrec
    def duplicateWithoutRepetitionIter(toDuplicate: Set[A], duplications: Queue[Int], returnQueue: Queue[A], howMany: Int, element: A): Queue[A] = {
      if (howMany > 0) duplicateWithoutRepetitionIter(toDuplicate, duplications, returnQueue.enqueue(element), howMany - 1, element)
      else if (toDuplicate.isEmpty || duplications.isEmpty) returnQueue
      else {
        val (newHowMany, tail2) = duplications.dequeue
        duplicateWithoutRepetitionIter(toDuplicate.tail, tail2, returnQueue, newHowMany, toDuplicate.head)
      }
    }
    if (toDuplicate.isEmpty || duplications.isEmpty) Queue()
    else {
      val (howMany, tail2) = duplications.dequeue
      duplicateWithoutRepetitionIter(toDuplicate.tail, tail2, Queue(), howMany, toDuplicate.head)
    }
  }

  trait Debug {
    //task3 (5p)
    def printName(): Unit = println("Class: " + this.getClass.getSimpleName)

    //task4(5p)
    def printVars(): Unit = {
      var fields = this.getClass.getDeclaredFields
      var currentClass = this.getClass.getSuperclass
      //inherited fields
      while (currentClass != null) {
        fields = fields.appendedAll(currentClass.getDeclaredFields)
        currentClass = currentClass.getSuperclass
      }
      for (field <- fields) {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType.getName + ", " + field.get(this))
      }
    }

    //task5(5pkt)
    def debugName(): String = this.getClass.getSimpleName

    def debugVars(): Map[String, (Class[_], Any)] = {
      var map = Map[String, (Class[_], Any)]()
      var fields = this.getClass.getDeclaredFields
      var currentClass = this.getClass.getSuperclass
      //inherited fields
      while (currentClass != null) {
        fields = fields.appendedAll(currentClass.getDeclaredFields)
        currentClass = currentClass.getSuperclass
      }
      for (field <- fields) {
        field.setAccessible(true)
        map += (field.getName -> (field.getType, field.get(this)))
      }
      map
    }
  }
}
