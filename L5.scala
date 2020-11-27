import scala.annotation.tailrec
import scala.collection.immutable.Queue

class L5 {

  // zadanie 1 (2.5 pkt)
  // Queue:
  // dodanie nowego elementu na koniec - O(1)
  // usunięcie i zwrócenie pierwszego elementu - O(1)
  def duplicate[A](collection: Queue[A], duplications: Queue[Int]): Queue[A] ={
    @tailrec
    def replicate(element:A, times: Int, queue: Queue[A]):Queue[A] =
      if(times > 0) replicate(element, times - 1, queue.enqueue(element))
      else queue

    @tailrec
    def iteration(collection: Queue[A], duplications: Queue[Int], newQueue: Queue[A]):Queue[A] = {
      if(collection.isEmpty || duplications.isEmpty) newQueue
      else {
        val ((elem, queue1), (times, queue2)) = (collection.dequeue, duplications.dequeue)
        iteration(queue1, queue2, replicate(elem, times, newQueue))
      }
    }
    iteration(collection,duplications,Queue.empty)
  }

  // zadanie 2 (2.5 pkt)
  def duplicateOnce[A](collection: Queue[A], duplications: Queue[Int]): Queue[A] =
    duplicate(collection.distinct, duplications)

}

trait Debug{

  // zadanie 3 (5 pkt)
  def debugName(): Unit = println("Class: " + getClass.getName)

  // zadanie 4 (5 pkt)
  def debugVars(): Unit ={
    val array = getClass.getDeclaredFields
    for(field <- array){
      field.setAccessible(true)
      println("Var: " + field.getName+" => "+ field.getAnnotatedType + ", " + field.get(this))
    }
  }

  // zadanie 5 (5 pkt)
  def getClassName: String = getClass.getName

  def getFields: Array[(String,Class[_],AnyRef)] ={
    val array = getClass.getDeclaredFields
    val newArray = new Array[(String,Class[_],AnyRef)](array.length)
    var i = 0

    for(field <- array){
      field.setAccessible(true)
      val element = (field.getName, field.getType, field.get(this))
      newArray(i) = element
      i+=1
    }
    newArray
  }

}
