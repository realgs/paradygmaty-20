import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
// Karol Waliszewski

// Tests
var I = 0;

def modifyNumber(step: Int, quantity: Int): Unit = {
  var i = 1;
  while (i <= quantity) {
    Thread.sleep(100)
    I = i * step
    i = i + 1
  }
}

println(I)

val p1 = Future {
  modifyNumber(1, 10);
}
val p2 = Future {
  modifyNumber(10, 10);
}

println(I)
Await.result(p1, Duration.Inf)
println(I)
Await.result(p2, Duration.Inf)
println(I)

// Fibonacci
def fib(n: Int):Int =
  if(n < 0)
    -1
  else
    n match {
      case 0 => 1
      case 1 => 1
      case _ => fib(n-1) + fib(n-2)
    }

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)
fib(8)
fib(9)
fib(10)
//fib(30)

def fibPar(n: Int):Int =
  if(n < 0)
    -1
  else
    n match {
      case 0 => 1
      case 1 => 1
      case _ => {
        val f1 = Future{fib(n-1)}
        val f2 = Future{fib(n-2)}
        val r1 = Await.result(f1, Duration.Inf)
        val r2 = Await.result(f2, Duration.Inf)
        r1 + r2
      }
    }

fibPar(0)
fibPar(1)
fibPar(2)
fibPar(3)
fibPar(4)
fibPar(5)
fibPar(6)
fibPar(7)
fibPar(8)
fibPar(9)
fibPar(10)
//fibPar(30)

// Binary tree
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
    val lf = Future{generateTree(depth - 1, range)}
    val rf = Future{generateTree(depth - 1, range)}

    val rl = Await.result(lf, Duration.Inf)
    val rr = Await.result(rf, Duration.Inf)

    Node(getRandomNumber(range), rl, rr)
  }
}

generateTree(10, (1,10))

generateTreePar(10, (1,10))

// Sorting
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
    Future{quick(tab, left, j)}
    Future{quick(tab, i, right)}
  }
}

def quicksort(tab: Array[Int]):Unit =
  quick(tab, 0, tab.length - 1)

def quicksortPar(tab: Array[Int]):Unit =
  quickPar(tab, 0, tab.length - 1)

var arr = Array(1, 80, 55, 60, 20, 120, 5)
quicksort(arr)
arr

// Duplicates
def duplicate[A] (elements: List[A])(counters: List[Int]): List[A] = {
  elements match {
    case Nil => Nil
    case _ =>
    counters match {
      case 0::tail => duplicate(elements.tail)(tail)
      case hd::tail =>
        if(hd < 0)
          throw new Exception("Element cannot be duplicated n times if n < 0.")
        else
          elements.head :: duplicate(elements)((hd-1)::tail)
    }
  }
}

def duplicatePar[A] (elements: List[A])(counters: List[Int]): List[A] = {
  elements match {
    case Nil => Nil
    case _ =>
      counters match {
        case 0::tail => duplicate(elements.tail)(tail)
        case hd::tail =>
          if(hd < 0)
            throw new Exception("Element cannot be duplicated n times if n < 0.")
          else
            elements.head :: duplicate(elements)((hd-1)::tail)
      }
  }
}

duplicate(List(1,2,3,4,5))(List(1,2,3,4,5))

duplicatePar(List(1,2,3,4,5))(List(1,2,3,4,5))