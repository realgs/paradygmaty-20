package Lista5

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Lista5 {

  def duplicate[A](elementCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    @tailrec
    def innerDuplicate(elements: Queue[A], repetitions: Queue[Int], retCollection: Queue[A], currentCounter: Int): Queue[A] = {
      if (currentCounter > 0 ){
        innerDuplicate(elements, repetitions, retCollection.enqueue(elements.front), currentCounter-1)
      }
      else {
        val repetitionDequeue = repetitions.dequeue
        val elementsDequeue = elements.dequeue
        if (repetitionDequeue._2.isEmpty || elementsDequeue._2.isEmpty){
          retCollection
        }
        else{
          innerDuplicate(elementsDequeue._2, repetitionDequeue._2, retCollection, repetitionDequeue._2.front)
        }
      }
    }
    if(elementCollection.isEmpty || repetitionsCollection.isEmpty) {
      Queue()
    }
    else{
      innerDuplicate(elementCollection, repetitionsCollection, Queue(), repetitionsCollection.front)
    }
  }





}
