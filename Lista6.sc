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

def quicksortSequential(tab: Array[Int]): Unit  = {
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
  quick(tab)(0)(tab.length - 1)
}

def quicksortParallel(tab: Array[Int]): Unit  = {
  def quickParallel(tab: Array[Int])(length1: Int)(length2: Int): Unit =
    if (length1 < length2) {
      val (i, j) = partition(tab)(length1)(length2)
      if (j - length1 < length2 - i) {
        parallelObject.parallel(quickParallel(tab)(length1)(j),quickParallel(tab)(i)(length2))
      }
      else {
        parallelObject.parallel(quickParallel(tab)(i)(length2),quickParallel(tab)(length1)(j))
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
  val tempInt = new Array[Int](tab.size)

  def mergeSortHelp(startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) return
    val middleIndex = (startIndex + endIndex)/2
    mergeSortHelp(startIndex,middleIndex)
    mergeSortHelp(middleIndex + 1,endIndex)
    merge(tab,tempInt,startIndex,middleIndex,endIndex)
  }
  mergeSortHelp(0,tab.size - 1)
}

def mergeSortParallel(tab: Array[Int]): Unit = {
  val tempInt = new Array[Int](tab.size)

  def mergeSortHelp(startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) return
    val middleIndex = (startIndex + endIndex)/2
    parallelObject.parallel(mergeSortHelp(startIndex,middleIndex),mergeSortHelp(middleIndex + 1,endIndex))
    merge(tab,tempInt,startIndex,middleIndex,endIndex)
  }
  mergeSortHelp(0,tab.size - 1)
}

// Fibonacci *****************************************************************************************

def fibonacciSequential(n: Int): Int = {
  n match {
    case 0 => n
    case 1 => n
    case _ => fibonacciSequential(n - 1) + fibonacciSequential(n - 2)
  }
}

def fibonacciParallel(n: Int): Int = {
  n match {
    case 0 => 0
    case 1 => 1
    case _ => {
      val (a,b) = parallelObject.parallel(fibonacciParallel(n - 1), fibonacciParallel(n - 2))
      a+b
    }
  }
}

/* Całka MonteCarlo ***********************************************************************************
Całka monte carlo definiowana jest przez 3 kroki:
losujemy niezależne liczby u1,u2,u3...un z rozkładu jednostajnego [0,1]
przekształcamy xk = a + (b - a)uk dla k = 1,2,3,...N, gdzie b to górna granica całkowania, a - dolna, N - liczba punktów
jako przybliżoną wartośc całki uznajemy [(b-a)/n] * [suma od 1 do N f(xk)]
W poniższym programie b = x2, a = x1, numberOfPointsToCreate = N
 */

def sumFunctionValues(f: (Double) => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  val rand = new Random
  def sumFunctionValuesHelp(sum: Double, numberOfGeneratedPoints: Int): Double = {
    if (numberOfGeneratedPoints == numberOfPointsToCreate) sum
    else {
      val randomx = x1 + rand.nextDouble() * (x2 - x1)
      sumFunctionValuesHelp(sum + f(randomx), numberOfGeneratedPoints + 1)
    }
  }
  sumFunctionValuesHelp(0,0)
}

def integralSequential(f: (Double) => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  if (x1 > x2) integralSequential(f,x2,x1,numberOfPointsToCreate)
  (x2-x1)/numberOfPointsToCreate * sumFunctionValues(f,x1,x2,numberOfPointsToCreate)
}

def integralParallel(f: (Double) => Double, x1: Double, x2: Double, numberOfPointsToCreate: Int): Double = {
  if (x1 > x2) integralParallel(f,x2,x1,numberOfPointsToCreate)
  val middle = x1 + (x2-x1)/2
  val halfOfPoints = numberOfPointsToCreate/2
  val (sum1,sum2) = parallelObject.parallel(
    sumFunctionValues(f,x1,middle,halfOfPoints),
    sumFunctionValues(f,middle,x2,halfOfPoints))
  (x2-x1)/numberOfPointsToCreate * (sum1+sum2)
}

val t1 = Array(4,8,1,12,7,3,1,9)
val t2 = Array(4,8,1,12,7,3,1,9)
val t3 = Array(4,8,1,12,7,3,1,9)
val t4 = Array(4,8,1,12,7,3,1,9)
val toFib: Int = 42
quicksortSequential(t1)
quicksortParallel(t2)
mergeSortSequential(t3)
mergeSortParallel(t4)
fibonacciSequential(toFib)
fibonacciParallel(toFib)
t1
t2
t3
t4
integralSequential((n:Double)=> n*n,2,5,100000)
integralParallel((n:Double)=> n*n,2,5,100000)

