
import scala.collection.mutable
import Lista5.duplicate
import Lista5.duplicate2

object TestsForLista5 extends App {

  var testDequeue1 = new mutable.ArrayDeque[Int]()
  testDequeue1.append(1)
  testDequeue1.append(2)
  testDequeue1.append(3)

  println("Tests for zad 1")
  println(duplicate(testDequeue1, List(-1, 0, 2)) == mutable.ArrayDeque(3, 3))
  println(duplicate(testDequeue1, List(1, 2, 3)) == mutable.ArrayDeque(1, 2, 2, 3, 3, 3))
  println(duplicate(testDequeue1, List(1, 2, 3, 4, 5)) == mutable.ArrayDeque(1, 2, 2, 3, 3, 3))
  println(duplicate(testDequeue1, List(1, 2)) == mutable.ArrayDeque(1, 2, 2))

  println("\nTests for zad 2")
  var testDequeue2 = new mutable.ArrayDeque[Char]()
  testDequeue2.append('a')
  testDequeue2.append('l')
  testDequeue2.append('a')

  try {
    println(duplicate2(testDequeue1, List(1, 2, 3)) == mutable.ArrayDeque(1, 2, 2, 3, 3, 3))
  } catch {
    case e: Exception => println("Duplicate exception captured")
  }

  try {
    println(duplicate2(testDequeue2, List(1, 2, 3)) == mutable.ArrayDeque('a', 'l', 'l', 'a', 'a', 'a'))
  } catch {
    case e: Exception => println("Duplicate exception captured")
  }


  var testDebug1 = new TestClass1
  var testDebug2 = new TestClass2

  println("\nTests for zad 3")
  testDebug1.debugName()
  testDebug2.debugName()

  println("\nTests for zad 4")
  testDebug1.debugVars()

  println("")
  testDebug2.debugVars()

  println("\nTests for zad 5")
  println(testDebug1.debugGetClassName()=="TestClass1")
  println(testDebug1.debugGetVarsArray()(0)(0))
  println(testDebug1.debugGetVarsArray()(0)(1))
  println(testDebug1.debugGetVarsArray()(0)(2))

  println("\n" + (testDebug2.debugGetClassName()=="TestClass2"))
  println(testDebug2.debugGetVarsArray()(0)(0))
  println(testDebug2.debugGetVarsArray()(0)(1))
  println(testDebug2.debugGetVarsArray()(0)(2))

}
