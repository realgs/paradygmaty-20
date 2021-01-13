import L5._

import scala.collection.immutable.Queue
object L5Test {
  def main(args:Array[String]):Unit={
    // Testy zadanie 1 2.5pkt.
    testyZadanie1()
    // Testy zadanie 2 2.5pkt.
    testyZadanie2()
    // Testy zadanie 3 5pkt.
    testyZadanie3()
    // Testy zadanie 4 5pkt.
    testyZadanie4()
    // Testy zadanie 5 5pkt.
    testyZadanie5()
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
  def testyZadanie3():Unit={
    val p = new Point(1,2)
    val o = new Circle(p,2)
    val e = new EmptyClass()

    print("Test 1 Zadanie 3: ")
    p.debugName()

    print("Test 2 Zadanie 3: ")
    o.debugName()

    print("Test 3 Zadanie 3: ")
    e.debugName()
    println()
  }
  def testyZadanie4():Unit={
    val p = new Point(1,2)
    val o = new Circle(p,2)
    val e = new EmptyClass()

    println("Test 1 Zadanie 4: ")
    p.debugVars()
    println()
    println("Test 2 Zadanie 4: ")
    o.debugVars()
    println()
    println("Test 3 Zadanie 4: ")
    e.debugVars()
    println()
  }
  def testyZadanie5():Unit={
    val p = new Point(1,2)
    val o = new Circle(p,2)
    val e = new EmptyClass()

    val (n0,c0,v0) = p.getDebugVars(0)
    val (n1,c1,v1) = p.getDebugVars(1)
    val (n2,c2,v2) = p.getDebugVars(2)
    val test1 = p.getDebugName == "Point" && (n0 == "x" && c0.getSimpleName == "int" && v0 == 1) &&
      (n1 == "y" && c1.getSimpleName == "int" && v1 == 2) &&
      (n2 == "a" && c2.getSimpleName == "String" && v2 == "test") && p.getDebugVars.length == 3
    println("Test 1 Zadanie 5: " + test1)

    val (n3,c3,_) = o.getDebugVars(0)
    val (n4,c4,v4) = o.getDebugVars(1)
    val test2 = o.getDebugName == "Circle" && (n3 == "o" && c3.getSimpleName == "Point") &&
      (n4 == "r" && c4.getSimpleName == "int" && v4 == 2) && o.getDebugVars.length == 2
    println("Test 2 Zadanie 5: " + test2)

    val test3 = e.getDebugName == "EmptyClass" && e.getDebugVars.length == 0
    println("Test 3 Zadanie 5: " + test3)
  }
}

