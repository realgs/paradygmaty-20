import L5._

import scala.collection.immutable.Queue
object L5Test {
  def main(args:Array[String]):Unit={
    // Testy zadanie 1 2.5pkt.
    testyZadanie1()
    // Testy zadanie 2 2.5pkt.
    testyZadanie2()
  }

  def testyZadanie1():Unit={
    println("Test 1 zadanie 1: " + (duplicate(Queue(),Queue(1,2,3)) == Queue()))
    println("Test 2 zadanie 1: " + (duplicate(Queue(1,2,3),Queue()) == Queue()))
    println("Test 3 zadanie 1: " + (duplicate(Queue(),Queue()) == Queue()))
    println("Test 4 zadanie 1: " + (duplicate(Queue(1,2,3),Queue(-1,-2,3)) == Queue(3,3,3)))
    println("Test 5 zadanie 1: " + (duplicate(Queue("A","B","C"),Queue(1,2,1)) == Queue("A","B","B","C")))
    println("Test 6 zadanie 1: " + (duplicate(Queue(1,2,3),Queue(1,2)) == Queue(1,2,2)))
    println("Test 7 zadanie 1: " + (duplicate(Queue(1,2),Queue(1,2,3)) == Queue(1,2,2))+"\n")
  }
  def testyZadanie2():Unit={
    println("Test 1 zadanie 2: " + (duplicateDistinct(Queue(),Queue(1,2,3)) == Queue()))
    println("Test 2 zadanie 2: " + (duplicateDistinct(Queue(1,2,3),Queue()) == Queue()))
    println("Test 3 zadanie 2: " + (duplicateDistinct(Queue(),Queue()) == Queue()))
    println("Test 4 zadanie 2: " + (duplicateDistinct(Queue(3,3,3,1,2),Queue(-1,-2,3)) == Queue(2,2,2)))
    println("Test 5 zadanie 2: " + (duplicateDistinct(Queue("A","A","A","B","B","B","C","C","C"),Queue(1,2,1))
      == Queue("A","B","B","C")))
    println("Test 6 zadanie 2: " + (duplicateDistinct(Queue(1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3),Queue(1,2)) == Queue(1,2,2)))
    println("Test 7 zadanie 2: " + (duplicateDistinct(Queue(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2),Queue(1,2,3)) == Queue(1,2,2))+"\n")
  }
}
