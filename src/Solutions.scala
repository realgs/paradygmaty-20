import java.lang.reflect.{AnnotatedElement, Field}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Solutions {
  // Task 1 (2.5p)
  // Queue aC aC C C (head, tail, prepend, append)
  def duplicate[A] (collection: Queue[A], repetitions: Queue[Int]): Queue[A] = {
    @tailrec
    def replicate(queue: Queue[A], element: A, repsQueue: Queue[Int], reps: Int, result: Queue[A]): Queue[A] =
      if(reps > 0) replicate(queue, element, repsQueue, reps - 1, result.enqueue(element))
      else if(queue.nonEmpty && repsQueue.nonEmpty) {
        val (nextElement, currentQueue) = queue.dequeue
        val (nextRepCount, currentRepetitions) = repsQueue.dequeue
        replicate(currentQueue, nextElement, currentRepetitions, nextRepCount, result)
      }
      else result

    if (collection.isEmpty || repetitions.isEmpty) Queue.empty // instead of throwing an Exception for reps, let the function work as long as it's possible
    else {
      val (nextElement, currentQueue) = collection.dequeue
      val (nextRepCount, currentRepetitions) = repetitions.dequeue
      replicate(currentQueue, nextElement, currentRepetitions, nextRepCount, Queue().empty)
    }
  }

  // Task 2 (2.5p)
  def duplicateWithoutReps[A] (collection: Set[A], repetitions: Queue[Int]): Queue[A] = {
    @tailrec
    def replicate(elements: Set[A], element: A, repsQueue: Queue[Int], reps: Int, result: Queue[A]): Queue[A] =
      if(reps > 0) replicate(elements, element, repsQueue, reps - 1, result.enqueue(element))
      else if(elements.nonEmpty && repsQueue.nonEmpty) {
        val (nextRepCount, currentRepetitions) = repsQueue.dequeue
        replicate(elements.tail, elements.head, currentRepetitions, nextRepCount, result)
      }
      else result

    if (collection.isEmpty || repetitions.isEmpty) Queue.empty
    else {
      val (nextRepCount, currentRepetitions) = repetitions.dequeue
      replicate(collection.tail, collection.head, currentRepetitions, nextRepCount, Queue().empty)
    }
  }

  // Task 3 + 4 + 5 (5p + 5p + 5p)
  trait Debug {
    // Task 3 (5p)
    def debugName(): Unit = println("Class: " + getClass.getSimpleName)

    // Task 4 (5p)
    def debugVars(): Unit = {
      for(field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
      }
    }

    // Task 5 (5p)
    def getName: String = getClass.getSimpleName
    def getFields: Map[String, (AnnotatedElement, AnyRef)] = {
      var map: Map[String, (AnnotatedElement, AnyRef)] = Map()
      for(field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        map = map + (field.getName -> (field.getType, field.get(this)))
      }
      map
    }
  }
}

