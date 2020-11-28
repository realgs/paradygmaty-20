package Lista5

import java.lang.reflect.{AnnotatedElement, Field}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.immutable.HashSet

object Lista5 extends App {

  //zadanie 1 (2.5 pkt)
  //complexity of Queue methods: enqueue, dequeue, front - O(1)
  def duplicate[A](elementCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    @tailrec
    def innerDuplicate(elements: Queue[A], repetitions: Queue[Int], retCollection: Queue[A], currentCounter: Int): Queue[A] = {
      if (currentCounter > 0 )
        innerDuplicate(elements, repetitions, retCollection.enqueue(elements.front), currentCounter-1)
      else {
        val repetitionDequeue = repetitions.dequeue
        val elementsDequeue = elements.dequeue
        if (repetitionDequeue._2.isEmpty || elementsDequeue._2.isEmpty)
          retCollection
        else
          innerDuplicate(elementsDequeue._2, repetitionDequeue._2, retCollection, repetitionDequeue._2.front)
      }
    }

    if(elementCollection.isEmpty || repetitionsCollection.isEmpty)
      Queue()
    else
      innerDuplicate(elementCollection, repetitionsCollection, Queue(), repetitionsCollection.front)
  }


  //zadanie 2 (2.5 pkt)
  //complexity of Queue methods: enqueue, dequeue, front : O(1)
  //complexity of hashSet methods: contains, + : O(1)
  def duplicateWithoutRepeating[A](elementCollection: Queue[A], repetitionsCollection: Queue[Int]): Queue[A] = {
    if(elementCollection.isEmpty || repetitionsCollection.isEmpty)
      Queue()
    else {
      @tailrec
      //delete repetitive elements from elements queue and its corresponding repetition value from repetitions queue
      def innerPrepare(elements: Queue[A], repetitions: Queue[Int], newElements: Queue[A], newRepetition: Queue[Int], checkSet: HashSet[A]): (Queue[A], Queue[Int]) = {
        if (elements.isEmpty || repetitions.isEmpty)
          (newElements, newRepetition)
        else {
          if (checkSet.contains(elements.front))
            innerPrepare(elements.dequeue._2, repetitions.dequeue._2, newElements, newRepetition, checkSet)
          else {
            val repetitionDequeue = repetitions.dequeue
            val elementsDequeue = elements.dequeue
            innerPrepare(elementsDequeue._2, repetitionDequeue._2, newElements.enqueue(elementsDequeue._1), newRepetition.enqueue(repetitionDequeue._1), checkSet+elementsDequeue._1)
          }
        }
      }
      val processedQueues = innerPrepare(elementCollection, repetitionsCollection, Queue(), Queue(), HashSet())
      duplicate(processedQueues._1, processedQueues._2)
    }
  }


  //classes used for testing exc. 3, 4, and 5
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Car(brandX: String, dateOfProductionX: Int) extends Debug {
    var brand: String = brandX
    var dateOfProduction: Int = dateOfProductionX
  }

  class Garage(carX: Car, areaX: Int) extends Debug {
    var car: Car = carX
    var area: Int = areaX
  }


  trait Debug {
    //zadanie 3 (5 pkt)
    def debugName(): Unit =
      println("Class: " + getClass.getSimpleName)

    //zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getAnnotatedType + ", " + field.get(this))
      }
    }

    //zadanie 5 (5 pkt)
    def debugGetName(): String =
      getClass.getSimpleName

    def debugGetVars(): Array[(String, AnnotatedElement, Object)] = {
      val fieldsArray: Array[Field] = getClass.getDeclaredFields
      val retArray: Array[(String, AnnotatedElement, Object)] = new Array[(String, AnnotatedElement, Object)](fieldsArray.length)
      var i = 0
      for (field <- fieldsArray) {
        field.setAccessible(true)
        retArray(i) = (field.getName, field.getAnnotatedType, field.get(this))
        i += 1
      }
      retArray
    }
  }

}
