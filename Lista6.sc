import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread, RecursiveTask}
import scala.util.Random

// Metody narzędziowe jak z artykulu **********************************************************************

object parallelObject {
  val forkJoinPool = new ForkJoinPool
  val scheduler = new TaskScheduler

  class TaskScheduler {
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }

    def schedule[T](body: => T): RecursiveTask[T] = {
      val task = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case _: ForkJoinWorkerThread => task.fork()
        case _ => forkJoinPool.execute(task)
      }
      task
    }
  }

  def task[T](body: => T): RecursiveTask[T] = {
    scheduler.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.parallel(taskA, taskB)
  }
}

// Quicksort ***************************************************************************************

def swap[A](tab: Array[A])(num1: Int)(num2: Int): Unit = {
  val tempElem = tab(num1)
  tab(num1) = tab(num2)
  tab(num2) = tempElem
}

def partition(tab: Array[Int])(length1: Int)(length2: Int): (Int,Int)= {
  var i = length1
  var j = length2
  val pivot = tab((length1+length2)/2)
  while (i <= j) {
    while (tab(i) < pivot) i += 1
    while (pivot < tab(j)) j -= 1
    if (i <= j) {
      swap(tab)(i)(j)
      i += 1
      j -= 1
    }
  }
  (i, j)
}

def quick(tab: Array[Int])(length1: Int)(length2: Int): Unit =
  if (length1 < length2) {
    val (i, j) = partition(tab)(length1)(length2)
    if (j - length1 < length2 - i) {
      quick(tab)(length1)(j)
      quick(tab)(i)(length2)
    }
    else {
      quick(tab)(i)(length2)
      quick(tab)(length1)(j)
    }
  }

def quicksortSequential(tab: Array[Int]): Unit  = {
  quick(tab)(0)(tab.length - 1)
}

val QUICKSORTTHRESHOLD = 5000
def quicksortParallel(tab: Array[Int]): Unit  = {
  def quickParallel(tab: Array[Int])(length1: Int)(length2: Int): Unit =
    if (length1 < length2) {
      if (length2 - length1 > QUICKSORTTHRESHOLD) {
          val (i, j) = partition(tab)(length1)(length2)
          if (j - length1 < length2 - i) {
            parallelObject.parallel(quickParallel(tab)(length1)(j), quickParallel(tab)(i)(length2))
          }
          else {
            parallelObject.parallel(quickParallel(tab)(i)(length2), quickParallel(tab)(length1)(j))
          }
      } else {
          val (i, j) = partition(tab)(length1)(length2)
          if (j - length1 < length2 - i) {
            quick(tab)(length1)(j)
            quick(tab)(i)(length2)
          }
          else {
            quick(tab)(i)(length2)
            quick(tab)(length1)(j)
          }
      }
    }
  quickParallel(tab)(0)(tab.length - 1)
}

// MergeSort *****************************************************************************************

def merge(tab: Array[Int], tempInt: Array[Int], startIndex: Int, middleIndex: Int, endIndex: Int): Unit = {
  for (i <- startIndex to endIndex) tempInt(i) = tab(i)

  var i = startIndex
  var j = middleIndex + 1
  var k = startIndex

  while ((i <= middleIndex) && (j <= endIndex)) {
    if (tempInt(i) < tempInt(j)) {
      tab(k) = tempInt(i)
      i += 1
    } else {
      tab(k) = tempInt(j)
      j += 1
    }
    k += 1
  }

  while (i <= middleIndex) {
    tab(k) = tempInt(i)
    k += 1
    i += 1
  }

  while (j <= endIndex) {
    tab(k) = tempInt(j)
    k += 1
    j += 1
  }
}

def mergeSortSequential(tab: Array[Int]): Unit = {
  val tempInt = new Array[Int](tab.length)

  def mergeSortHelp(startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) return
    val middleIndex = (startIndex + endIndex)/2
    mergeSortHelp(startIndex,middleIndex)
    mergeSortHelp(middleIndex + 1,endIndex)
    merge(tab,tempInt,startIndex,middleIndex,endIndex)
  }
  mergeSortHelp(0,tab.length - 1)
}

def mergeSortParallel(tab: Array[Int]): Unit = {
  val tempInt = new Array[Int](tab.length)
  val THRESHOLD = 8192

  def mergeSortHelp(startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) return
    val middleIndex = (startIndex + endIndex) / 2
    if (endIndex - startIndex <= THRESHOLD) {
      mergeSortHelp(startIndex,middleIndex)
      mergeSortHelp(middleIndex + 1,endIndex)
    } else {
      parallelObject.parallel(mergeSortHelp(startIndex, middleIndex), mergeSortHelp(middleIndex + 1, endIndex))
    }
    merge(tab, tempInt, startIndex, middleIndex, endIndex)
  }
  mergeSortHelp(0,tab.length - 1)
}

// Fibonacci *****************************************************************************************
val THRESHOLDFIB = 23
def fibonacciSequential(n: Int): Int = {
  if (n < 0) throw new Exception("argument less than zero")
  n match {
    case 0 => n
    case 1 => n
    case _ => fibonacciSequential(n - 1) + fibonacciSequential(n - 2)
  }
}

def fibonacciParallel(n: Int): Int = {
  if (n < 0) throw new Exception("argument less than zero")
  n match {
    case 0 => 0
    case 1 => 1
    case _ => if (n > THRESHOLDFIB) {
                val (a, b) = parallelObject.parallel(fibonacciParallel(n - 1), fibonacciParallel(n - 2))
                a + b
              } else fibonacciSequential(n - 1) + fibonacciSequential(n - 2)
  }
}

/* Całka MonteCarlo ***********************************************************************************
Całka monte carlo definiowana jest przez 3 kroki:
losujemy niezależne liczby u1,u2,u3...un z rozkładu jednostajnego [0,1]
przekształcamy xk = a + (b - a)uk dla k = 1,2,3,...N, gdzie b to górna granica całkowania, a - dolna, N - liczba punktów
jako przybliżoną wartośc całki uznajemy [(b-a)/n] * [suma od 1 do N f(xk)]
W poniższym programie b = x2, a = x1, numberOfPointsToCreate = N
 */

def sumFunctionValues(f: Double => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  val rand = new Random
  def sumFunctionValuesHelp(sum: Double, numberOfGeneratedPoints: Int): Double =
    if (numberOfGeneratedPoints == numberOfPointsToCreate) sum
    else {
      val randomx = x1 + rand.nextDouble() * (x2 - x1)
      sumFunctionValuesHelp(sum + f(randomx), numberOfGeneratedPoints + 1)
    }
  sumFunctionValuesHelp(0,0)
}

def integralSequential(f: Double => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  if (x1 > x2) integralSequential(f,x2,x1,numberOfPointsToCreate)
  (x2-x1)/numberOfPointsToCreate * sumFunctionValues(f,x1,x2,numberOfPointsToCreate)
}

def integralParallel(f: Double => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  if (x1 > x2) integralParallel(f,x2,x1,numberOfPointsToCreate)
  val middle = x1 + (x2-x1)/2
  val halfOfPoints = numberOfPointsToCreate/2
  val (sum1,sum2) = parallelObject.parallel(
    sumFunctionValues(f,x1,middle,halfOfPoints),
    sumFunctionValues(f,middle,x2,halfOfPoints))
  (x2-x1)/numberOfPointsToCreate * (sum1+sum2)
}

//************************************************TESTY**********************************************
def execTimeOf[T](body: => T): Long = {
  val startTime = System.nanoTime()
  body
  val endTime = System.nanoTime()
  endTime - startTime
}

def correctnessTests(): Unit = {
  val arrayTest1 = Array(3,6,8,-8,9,3,414,21,5,-6,8,43,-31,2,1434,43,6,43,7)
  val arrayTest2 = arrayTest1.clone()
  val arrayTest3 = arrayTest1.clone()
  val arrayTest4 = arrayTest1.clone()
  val arrayTest5 = Array()

  quicksortSequential(arrayTest1)
  quicksortParallel(arrayTest2)
  mergeSortSequential(arrayTest3)
  mergeSortParallel(arrayTest4)

  assert(arrayTest1 sameElements Array(-31, -8, -6, 2, 3, 3, 5, 6, 6, 7, 8, 8, 9, 21, 43, 43, 43, 414, 1434))
  assert(arrayTest2 sameElements Array(-31, -8, -6, 2, 3, 3, 5, 6, 6, 7, 8, 8, 9, 21, 43, 43, 43, 414, 1434))
  assert(arrayTest3 sameElements Array(-31, -8, -6, 2, 3, 3, 5, 6, 6, 7, 8, 8, 9, 21, 43, 43, 43, 414, 1434))
  assert(arrayTest4 sameElements Array(-31, -8, -6, 2, 3, 3, 5, 6, 6, 7, 8, 8, 9, 21, 43, 43, 43, 414, 1434))
  assert(fibonacciSequential(5) == 5)
  assert(fibonacciSequential(16) == 987)
  assert(fibonacciSequential(28) == 317811)
  assert(fibonacciSequential(42) == 267914296)
  assert(fibonacciParallel(5) == 5)
  assert(fibonacciParallel(16) == 987)
  assert(fibonacciParallel(28) == 317811)
  assert(fibonacciParallel(42) == 267914296)

  val pointsToCreate = 1000000
  val function1 = (n:Double) => n*n
  val function2 = (n:Double) => (n*n+1)/n
  val function3 = (n:Double) => 1/(n*n)
  val function4 = (n:Double) => math.sin(n)
  val x1 = 2
  val x2 = 5

  println("Sprawdzenie calki Monte Carlo")
  println("Ma byc okolo: 39")
  println(integralSequential(function1,x1,x2,pointsToCreate))
  println(integralParallel(function1,x1,x2,pointsToCreate))
  println("Ma byc okolo: 11,416")
  println(integralSequential(function2,x1,x2,pointsToCreate))
  println(integralParallel(function2,x1,x2,pointsToCreate))
  println("Ma byc okolo: 0,3")
  println(integralSequential(function3,x1,x2,pointsToCreate))
  println(integralParallel(function3,x1,x2,pointsToCreate))
  println("Ma byc okolo: -0,69981")
  println(integralSequential(function4,x1,x2,pointsToCreate))
  println(integralParallel(function4,x1,x2,pointsToCreate))
}

def quickSortSpeedTest(): Unit = {
  val array100Elems = Array.fill(100)(Random.between(0,100000))
  val array1000Elems = Array.fill(1000)(Random.between(0,100000))
  val array100000Elems = Array.fill(100000)(Random.between(0,100000))
  val array1000000Elems = Array.fill(1000000)(Random.between(0,100000))
  val array100ElemsClone = array100Elems.clone()
  val array1000ElemsClone = array1000Elems.clone()
  val array100000ElemsClone = array100000Elems.clone()
  val array1000000ElemsClone = array1000000Elems.clone()
  val time1 = execTimeOf(quicksortSequential(array100Elems))
  val time2 = execTimeOf(quicksortParallel(array100ElemsClone))
  val time3 = execTimeOf(quicksortSequential(array1000Elems))
  val time4 = execTimeOf(quicksortParallel(array1000ElemsClone))
  val time5 = execTimeOf(quicksortSequential(array100000Elems))
  val time6 = execTimeOf(quicksortParallel(array100000ElemsClone))
  val time7 = execTimeOf(quicksortSequential(array1000000Elems))
  val time8 = execTimeOf(quicksortParallel(array1000000ElemsClone))

  println("QuickSort******************************************************************")
  println("Array with 100 elems")
  println("Sequential: " + time1 + " ns ")
  println("  Parallel: " + time2 + " ns " + "effectiveness: " + time1/time2*100 + "%")
  println("Array with 1000 elems")
  println("Sequential: " + time3 + " ns ")
  println("  Parallel: " + time4 + " ns " + "effectiveness: " + time3/time4*100 + "%")
  println("Array with 100000 elems")
  println("Sequential: " + time5 + " ns ")
  println("  Parallel: " + time6 + " ns " + "effectiveness: " + time5/time6*100 + "%")
  println("Array with 1000000 elems")
  println("Sequential: " + time7 + " ns ")
  println("  Parallel: " + time8 + " ns " + "effectiveness: " + time7/time8*100 + "%\n")
}

def mergeSortSpeedTest(): Unit = {
  val array100Elems = Array.fill(100)(Random.between(0,100000))
  val array1000Elems = Array.fill(1000)(Random.between(0,100000))
  val array100000Elems = Array.fill(100000)(Random.between(0,100000))
  val array1000000Elems = Array.fill(1000000)(Random.between(0,100000))
  val array100ElemsClone = array100Elems.clone()
  val array1000ElemsClone = array1000Elems.clone()
  val array100000ElemsClone = array100000Elems.clone()
  val array1000000ElemsClone = array1000000Elems.clone()
  val time1 = execTimeOf(mergeSortSequential(array100Elems))
  val time2 = execTimeOf(mergeSortParallel(array100ElemsClone))
  val time3 = execTimeOf(mergeSortSequential(array1000Elems))
  val time4 = execTimeOf(mergeSortParallel(array1000ElemsClone))
  val time5 = execTimeOf(mergeSortSequential(array100000Elems))
  val time6 = execTimeOf(mergeSortParallel(array100000ElemsClone))
  val time7 = execTimeOf(mergeSortSequential(array1000000Elems))
  val time8 = execTimeOf(mergeSortParallel(array1000000ElemsClone))

  println("MergeSort******************************************************************")
  println("Array with 100 elems")
  println("Sequential: " + time1 + " ns ")
  println("  Parallel: " + time2 + " ns " + "effectiveness: " + time1/time2*100 + "%")
  println("Array with 1000 elems")
  println("Sequential: " + time3 + " ns ")
  println("  Parallel: " + time4 + " ns " + "effectiveness: " + time3/time4*100 + "%")
  println("Array with 100000 elems")
  println("Sequential: " + time5 + " ns ")
  println("  Parallel: " + time6 + " ns " + "effectiveness: " + time5/time6*100 + "%")
  println("Array with 1000000 elems")
  println("Sequential: " + time7 + " ns ")
  println("  Parallel: " + time8 + " ns " + "effectiveness: " + time7/time8*100 + "%\n")
}

def fibonacciSpeedTest(): Unit = {
  val toFib1: Int = 5
  val toFib2: Int = 16
  val toFib3: Int = 28
  val toFib4: Int = 42

  val time1 = execTimeOf(fibonacciSequential(toFib1))
  val time2 = execTimeOf(fibonacciParallel(toFib1))
  val time3 = execTimeOf(fibonacciSequential(toFib2))
  val time4 = execTimeOf(fibonacciParallel(toFib2))
  val time5 = execTimeOf(fibonacciSequential(toFib3))
  val time6 = execTimeOf(fibonacciParallel(toFib3))
  val time7 = execTimeOf(fibonacciSequential(toFib4))
  val time8 = execTimeOf(fibonacciParallel(toFib4))

  println("Fibonacci******************************************************************")
  println("fibonacci of 5")
  println("Sequential: " + time1 + " ns ")
  println("  Parallel: " + time2 + " ns " + "effectiveness: " + time1/time2*100 + "%")
  println("fibonacci of 10")
  println("Sequential: " + time3 + " ns ")
  println("  Parallel: " + time4 + " ns " + "effectiveness: " + time3/time4*100 + "%")
  println("fibonacci of 20")
  println("Sequential: " + time5 + " ns ")
  println("  Parallel: " + time6 + " ns " + "effectiveness: " + time5/time6*100 + "%")
  println("fibonacci of 42")
  println("Sequential: " + time7 + " ns ")
  println("  Parallel: " + time8 + " ns " + "effectiveness: " + time7/time8*100 + "%\n")
}

def monteCarloIntegralSpeedTest():Unit = {
  val pointsToCreate = 1000000
  val function1 = (n:Double) => n*n
  val function2 = (n:Double) => (n*n+1)/n
  val function3 = (n:Double) => 1/(n*n)
  val function4 = (n:Double) => math.sin(n)
  val x1 = 2
  val x2 = 5

  val time1 = execTimeOf(integralSequential(function1,x1,x2,pointsToCreate))
  val time2 = execTimeOf(integralParallel(function1,x1,x2,pointsToCreate))
  val time3 = execTimeOf(integralSequential(function2,x1,x2,pointsToCreate))
  val time4 = execTimeOf(integralParallel(function2,x1,x2,pointsToCreate))
  val time5 = execTimeOf(integralSequential(function3,x1,x2,pointsToCreate))
  val time6 = execTimeOf(integralParallel(function3,x1,x2,pointsToCreate))
  val time7 = execTimeOf(integralSequential(function4,x1,x2,pointsToCreate))
  val time8 = execTimeOf(integralParallel(function4,x1,x2,pointsToCreate))

  println("Monte carlo integral******************************************************************")
  println("integral from" + x1 + "to" + x2 + "of" + function1)
  println("Sequential: " + time1 + " ns ")
  println("  Parallel: " + time2 + " ns " + "effectiveness: " + time1/time2*100 + "%")
  println("integral from" + x1 + "to" + x2 + "of" + function2)
  println("Sequential: " + time3 + " ns ")
  println("  Parallel: " + time4 + " ns " + "effectiveness: " + time3/time4*100 + "%")
  println("integral from" + x1 + "to" + x2 + "of" + function3)
  println("Sequential: " + time5 + " ns ")
  println("  Parallel: " + time6 + " ns " + "effectiveness: " + time5/time6*100 + "%")
  println("integral from" + x1 + "to" + x2 + "of" + function4)
  println("Sequential: " + time7 + " ns ")
  println("  Parallel: " + time8 + " ns " + "effectiveness: " + time7/time8*100 + "%\n")
}

correctnessTests()
quickSortSpeedTest()
mergeSortSpeedTest()
fibonacciSpeedTest()
monteCarloIntegralSpeedTest()

