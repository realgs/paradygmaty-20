import java.lang.reflect.Field

import scala.annotation.tailrec
import scala.collection.immutable.Queue
// Karol Waliszewski

// 1) - 2.5pkt
//def duplicate[A](elements: List[A])(counters: List[Int]):List[A] =
// elements match {
//   case Nil => Nil
//   case _ =>
//     counters match {
//       case Nil => Nil
//       case 0::tail => duplicate(elements.tail)(counters.tail)
//       case hd::tail =>
//         if(hd < 0)
//           throw new Exception("Element cannot be duplicated n times if n < 0.")
//         else
//          elements.head :: duplicate(elements)((hd-1)::tail)
//     }
// }

def duplicate[A] (collection: Queue[A])(repetitions: Queue[Int]): Queue[A] = {
  @tailrec
  def duplicateInner(elements: Queue[A], element: A, repetitions: Queue[Int], reps: Int, result: Queue[A]): Queue[A] =
    if(reps > 0) duplicateInner(elements, element, repetitions, reps - 1, result.enqueue(element))
    else if(elements.nonEmpty && repetitions.nonEmpty) {
      val (newElement, newElements) = elements.dequeue
      val (newReps, newRepetitions) = repetitions.dequeue
      duplicateInner(newElements, newElement, newRepetitions, newReps, result)
    }
    else result


  if(repetitions.isEmpty || collection.isEmpty) Queue.empty
  else {
    val (newElement, newElements) = collection.dequeue
    val (newReps, newRepetitions) = repetitions.dequeue
    duplicateInner(newElements, newElement, newRepetitions, newReps, Queue().empty)
  }
}

duplicate(Queue(1, 2, 3, 4))(Queue(1, 2, 3, 4))
duplicate(Queue(1, 2, 3))(Queue(0, 2, 3, 4))
duplicate(Queue(1, 2, 3))(Queue(0, 2))
duplicate(Queue(1))(Queue(0, 2))
duplicate(Queue())(Queue(2))
duplicate(Queue("a", "b"))(Queue(0, 2))
duplicate(Queue("a", "b", "c"))(Queue(2, 3))

// 2) - 2.5pkt
def duplicate2[A](elements: Set[A])(counters: Queue[Int]):Queue[A] = {
  var queue = Queue[A]();
  for(el <- elements){
    queue = queue.enqueue(el);
  }
  duplicate(queue)(counters)
}

duplicate2(Set(2,1,2,1,1))(Queue(1,2,3,4,5))

trait Debug {
  // 3) - 5pkt
  def debugName(): Unit = println("Class: " + this.getClass.getSimpleName)
  // 4) - 5pkt
  def debugVars(): Unit = {
    def printFields(fields: Array[Field]):Unit =
      fields.foreach(field => {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
      })

    val fields = this.getClass.getDeclaredFields
    printFields(fields.slice(0, fields.length - 1))
  }

  // 5) - 5pkt
  def getDebugName:String = this.getClass.getSimpleName

  def getDebugFields: Array[Field] = {
    val fields = this.getClass.getDeclaredFields
    fields.slice(0, fields.length - 1)
  }

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

val p : Point = new Point(3,4)
p.debugName()
p.debugVars()


p.getDebugName
p.getDebugFields