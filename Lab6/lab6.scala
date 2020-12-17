import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Quicksort {
  def swap[A](tab: Array[A], i: Int, j: Int):Unit = {
    val temp = tab(i)
    tab(i) = tab(j)
    tab(j) = temp
  }

  def partition(tab: Array[Int], left: Int, right: Int):(Int, Int) = {
    var i = left
    var j = right
    val pivot = tab((left + right) / 2)

    while(i <= j){
      while(tab(i) < pivot){
        i = i + 1
      }
      while(tab(j) > pivot){
        j = j - 1
      }
      if(i <= j) {
        swap(tab, i, j)
        i = i + 1
        j = j - 1
      }
    }

    (i, j)
  }

  def quick(tab: Array[Int], left: Int, right: Int):Unit = {
    if(left < right) {
      val (i, j) = partition(tab, left, right)
      if(j-left < right - i) {
        quick(tab, left, j)
        quick(tab, i, right)
      }else {
        quick(tab, i, right)
        quick(tab, left, j)
      }
    }
  }

  def quickPar(tab: Array[Int], left: Int, right: Int):Unit = {
    if(left < right) {
      val (i, j) = partition(tab, left, right)
      if(j-left < right - i) {
        Future{quick(tab, left, j)}
        Future{quick(tab, i, right)}
      }else {
        Future{quick(tab, i, right)}
        Future{quick(tab, left, j)}
      }
    }
  }

  def quicksort(tab: Array[Int]):Unit =
    quick(tab, 0, tab.length - 1)

  def quicksortPar(tab: Array[Int]):Unit =
    quickPar(tab, 0, tab.length - 1)
}

object Tree {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def getRandomNumber(range:(Int, Int)):Int = {
    if(range._1 <= 0 || range._2 <= 0)
      throw new Exception("Range have to be positive value.")

    if(range._2 < range._1)
      throw new Exception("Invalid range.")

    val rand = scala.util.Random
    rand.nextInt(range._2 - range._1 + 1) + range._1
  }

  def generateTree(depth: Int, range:(Int, Int)):BT[Int] = {
    if(depth <= 0)
      Empty
    else
      Node(getRandomNumber(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
  }

  def generateTreePar(depth: Int, range:(Int, Int)):BT[Int] = {
    if(depth <= 0)
      Empty
    else {
      val lf = Future{generateTreePar(depth - 1, range)}
      val rf = Future{generateTreePar(depth - 1, range)}

      val rl = Await.result(lf, Duration.Inf)
      val rr = Await.result(rf, Duration.Inf)

      Node(getRandomNumber(range), rl, rr)
    }
  }
}

object Fibonacci {
  def fib(n: Int):Int =
    if(n < 0)
      -1
    else
      n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n-1) + fib(n-2)
      }

  def fibPar(n: Int):Int =
    if(n < 0)
      -1
    else
      n match {
        case 0 => 0
        case 1 => 1
        case _ => {
          val f1 = Future{fib(n-1)}
          val f2 = Future{fib(n-2)}
          val r1 = Await.result(f1, Duration.Inf)
          val r2 = Await.result(f2, Duration.Inf)
          r1 + r2
        }
      }
}

object Lab6 {
  def getRandomNumber(range:(Int, Int)):Int = {
    if(range._1 <= 0 || range._2 <= 0)
      throw new Exception("Range have to be positive value.")

    if(range._2 < range._1)
      throw new Exception("Invalid range.")

    val rand = scala.util.Random
    rand.nextInt(range._2 - range._1 + 1) + range._1
  }

  // Time measure
  def timeNS[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def timeMS[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def main(args: Array[String]): Unit = {

    println("Quicksort:")
    // Tests for normal
    var arr1 = Array(1, 80, 55, 60, 20, 120, 5, -10)
    var arr2 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    var arr3 = Array(10, 9 ,8, 7, 6, 5, 4, 3, 2, 1)
    var arr4 = Array(0, 0, 0, 0, -1, 0, 0, 1, 0, 0)

    Quicksort.quicksort(arr1)
    Quicksort.quicksort(arr2)
    Quicksort.quicksort(arr3)
    Quicksort.quicksort(arr4)


    assert(arr1 sameElements Array(-10, 1, 5, 20, 55, 60, 80, 120)) // true
    assert(arr2 sameElements Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) // true
    assert(arr3 sameElements Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) // true
    assert(arr4 sameElements Array(-1, 0, 0, 0, 0, 0, 0, 0, 0, 1)) // true

    // Tests for parallel
    arr1 = Array(1, 80, 55, 60, 20, 120, 5, -10)
    arr2 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    arr3 = Array(10, 9 ,8, 7, 6, 5, 4, 3, 2, 1)
    arr4 = Array(0, 0, 0, 0, -1, 0, 0, 1, 0, 0)

    Quicksort.quicksortPar(arr1)
    Quicksort.quicksortPar(arr2)
    Quicksort.quicksortPar(arr3)
    Quicksort.quicksortPar(arr4)

    assert(arr1 sameElements Array(-10, 1, 5, 20, 55, 60, 80, 120)) // true
    assert(arr2 sameElements Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) // true
    assert(arr3 sameElements Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) // true
    assert(arr4 sameElements Array(-1, 0, 0, 0, 0, 0, 0, 0, 0, 1)) // true

    // Time measures
    val arr10000 = Array.fill(10000)(getRandomNumber(1, 1000000))
    val arr100000 = Array.fill(100000)(getRandomNumber(1, 1000000))
    val arr1000000 = Array.fill(1000000)(getRandomNumber(1, 1000000))
    val arr10000000 = Array.fill(10000000)(getRandomNumber(1, 1000000))

    println("10000:")
    timeNS {
      Quicksort.quicksort(arr10000.clone())
    } // 7315197 ns
    timeNS {
      Quicksort.quicksortPar(arr10000.clone())
    } // 291663 ns

    println("100000:")
    timeNS {
      Quicksort.quicksort(arr100000.clone())
    } // 38714981 ns
    timeNS {
      Quicksort.quicksortPar(arr100000.clone())
    } // 818133 ns

    println("1000000:")
    timeNS {
      Quicksort.quicksort(arr1000000.clone())
    } // 286614135 ns
    timeNS {
      Quicksort.quicksortPar(arr1000000.clone())
    } // 11189911 ns

    println("10000000:")
    timeNS {
      Quicksort.quicksort(arr10000000.clone())
    } // 2949002213 ns
    timeNS {
      Quicksort.quicksortPar(arr10000000.clone())
    } // 130160168 ns

    // Tree
    println("Tree:")

    println("3:")
    timeNS {
      Tree.generateTree(3, (1, 1000))
    } // 2073237 ns
    timeNS {
      Tree.generateTreePar(3, (1, 100))
    } // 25625714 ns

    println("5:")
    timeNS {
      Tree.generateTree(5, (1, 1000))
    } // 42867 ns
    timeNS {
      Tree.generateTreePar(5, (1, 100))
    } // 1308091346 ns

    println("10:")
    timeNS {
      Tree.generateTree(10, (1, 1000))
    } // 649749 ns
    timeNS {
      Tree.generateTreePar(10, (1, 100))
    } // 150223730 ns

    println("13:")
    timeNS {
      Tree.generateTree(13, (1, 1000))
    } // 978923 ns
    timeNS {
      Tree.generateTreePar(13, (1,100))
    } // 3321808511 ns


    // Fibonacci
    println("Fibonacci:")

    // Tests
    assert(Fibonacci.fib(0) == Fibonacci.fibPar(0) && Fibonacci.fibPar(0) == 0) // true
    assert(Fibonacci.fib(1) == Fibonacci.fibPar(1) && Fibonacci.fibPar(1) == 1) // true
    assert(Fibonacci.fib(2) == Fibonacci.fibPar(2) && Fibonacci.fibPar(2) == 1) // true
    assert(Fibonacci.fib(3) == Fibonacci.fibPar(3) && Fibonacci.fibPar(3) == 2) // true
    assert(Fibonacci.fib(4) == Fibonacci.fibPar(4) && Fibonacci.fibPar(4) == 3) // true
    assert(Fibonacci.fib(5) == Fibonacci.fibPar(5) && Fibonacci.fibPar(5) == 5) // true
    assert(Fibonacci.fib(6) == Fibonacci.fibPar(6) && Fibonacci.fibPar(6) == 8) // true
    assert(Fibonacci.fib(7) == Fibonacci.fibPar(7) && Fibonacci.fibPar(7) == 13) // true
    assert(Fibonacci.fib(12) == Fibonacci.fibPar(12) && Fibonacci.fibPar(12) == 144) // true

    // Measure
    println("5:")
    timeNS {
      Fibonacci.fib(5)
    } // 10218 ns
    timeNS {
      Fibonacci.fibPar(5)
    } // 73623 ns

    println("15:")
    timeNS {
      Fibonacci.fib(15)
    } // 27076 ns
    timeNS {
      Fibonacci.fibPar(15)
    } // 160626 ns

    println("25:")
    timeNS {
      Fibonacci.fib(25)
    } // 2064423 ns
    timeNS {
      Fibonacci.fibPar(25)
    } // 1380159 ns

    println("35:")
    timeNS {
      Fibonacci.fib(35)
    } // 88571852 ns
    timeNS {
      Fibonacci.fibPar(35)
    } // 51149966 ns

    println("45:")
    timeNS {
      Fibonacci.fib(45)
    } // 10183705830 ns
    timeNS {
      Fibonacci.fibPar(45)
    } // 6318791671 ns

    println("55:")
    timeNS {
      Fibonacci.fib(55)
    } // 1241777644598 ns
    timeNS {
      Fibonacci.fibPar(55)
    } // 769102332729 ns
  }
}





