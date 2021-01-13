import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

//Quick sorting
object QuickSort {
  def swap(tab:Array[Int])(i:Int)(j:Int):Unit ={
    val aux = tab(i)
    tab(i) = tab(j); tab(j) = aux
  }

  def choosePivot(tab:Array[Int])(m:Int)(n:Int):Int ={
    tab((m+n)/2)
  }

  def partition(tab:Array[Int])(l:Int)(r:Int):(Int,Int) ={
    var i = l; var j = r; val pivot = choosePivot(tab)(l)(r)
    while(i <= j){
      while(tab(i) < pivot) i+=1
      while(pivot < tab(j)) j-=1
      if(i <= j) {
        swap(tab)(i)(j); i+=1; j-=1
      }
    }
    (i,j)
  }

  def quick(tab:Array[Int])(l:Int)(r:Int):Unit ={
    if(l < r) {
      val (i, j) = partition(tab)(l)(r)
      if(j-l<r-i) {
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      }
      else{
        quick(tab)(i)(r)
        quick(tab)(l)(j)
      }
    }
    else ()
  }

  def quickParallel(tab:Array[Int])(l:Int)(r:Int):Unit = {
    if(l < r) {
      val (i, j) = partition(tab)(l)(r)
      if(j-l<r-i) {
        Future{quick(tab)(l)(j)}
        Future{quick(tab)(i)(r)}
      }
      else{
        Future{quick(tab)(i)(r)}
        Future{quick(tab)(l)(j)}
      }
    }
    else ()
  }

  def quickSort(tab:Array[Int]):Unit = quick(tab)(0)(tab.length-1)

  def quickSortParallel(tab:Array[Int]):Unit = quickParallel(tab)(0)(tab.length-1)
}

//Generating binary tree
object BinaryTree {
  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTree(n: Int, down: Int, up: Int): BT[Int] = {
    val r = scala.util.Random

    def helper(n: Int): BT[Int] =
      if (n == 0) Node(down + r.nextInt(up - down), Empty, Empty)
      else {
        val left = helper(n - 1)
        val right = helper(n - 1)
        Node(down + r.nextInt(up - down), left, right)
      }

    if (down < 0 || up < 0 || down > up || n <= 0) Empty
    else helper(n - 1)
  }

  def generateTreeParallel(n: Int, down: Int, up: Int): BT[Int] = {
    val r = scala.util.Random

    def helper(n: Int): BT[Int] =
      if (n == 0) Node(down + r.nextInt(up - down), Empty, Empty)
      else {
        val leftF = Future{helper(n - 1)}
        val rightF = Future{helper(n - 1)}

        val left = Await.result(leftF, Duration.Inf)
        val right = Await.result(rightF, Duration.Inf)

        Node(down + r.nextInt(up - down), left, right)
      }

    if (down < 0 || up < 0 || down > up || n <= 0) Empty
    else helper(n - 1)
  }
}

// Merge sorting
object MergeSort {
  @tailrec
  def reverse(list:List[Int], tail:List[Int]):List[Int]=
  if(list == Nil) tail
  else reverse(list.tail,list.head::tail)

  def divide(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def helper(list: List[Int], tail: List[Int], count: Int):(List[Int], List[Int]) ={
      if(count == 0) (reverse(list,Nil),tail)
      else
        helper(tail.head::list,tail.tail,count -1)
    }
    val m = list.length/2
    helper(Nil,list,m)
  }

  @tailrec
  def merge(left: List[Int], right: List[Int], newList: List[Int]):List[Int] =
    (left, right) match {
      case (left, Nil) => reverse(newList,left)
      case (Nil, right) => reverse(newList,right)
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) merge(leftTail, right,leftHead::newList)
        else merge(left, rightTail, rightHead::newList)
    }

  def mergeSort(list: List[Int]):List[Int] ={
    if(list.length == 1)
      list
    else if(list.length == 2){
      if(list.head > list.tail.head){
        list.tail.head::list.head::Nil
      }
      else
        list
    }
    else{
      val (left, right) = divide(list)
      val mergedLeft = mergeSort(left)
      val mergedRight = mergeSort(right)
      merge(mergedLeft, mergedRight,Nil)
    }
  }

  def mergeSortParallel(list: List[Int]):List[Int] ={
    if(list.length == 1)
      list
    else if(list.length == 2){
      if(list.head > list.tail.head){
        list.tail.head::list.head::Nil
      }
      else
        list
    }
    else{
      val (left, right) = divide(list)
      val mergedLeftP = Future{mergeSort(left)}
      val mergedRightP = Future{mergeSort(right)}
      merge(Await.result(mergedLeftP,Duration.Inf),Await.result(mergedRightP,Duration.Inf),Nil)
    }
  }
}

//Computing fibonacci numbers
object Fibonacci {
  def fibTail(n:Int):Int = {
    @tailrec
    def fibIter(f1: Int, f2: Int, m: Int): Int = {
      if (m == n) f1 + f2
      else fibIter(f2, f1 + f2, m + 1)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => if (n < 0) -1 else fibIter(0, 1, 2)
    }
  }

  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => if (n < 0) -1 else fibTail(n - 2) + fibTail(n - 1)
    }

  def fibParallel(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        if (n < 0) -1
        else {
          val fibLeftF = Future{fibTail(n - 2)}
          val fibRightF = Future{fibTail(n - 1)}

          Await.result(fibLeftF, Duration.Inf) + Await.result(fibRightF, Duration.Inf)
        }
    }
}

object Test {
  def main(args: Array[String]): Unit = {
    //quickSortTest()

    //binaryTreeTest()

    //fibonacciTest()

    //mergeSortTest()

  }

  def quickSortTest(): Unit ={
    val array = Array.fill(1000000)(Random.nextInt(100000))

    val t0 = System.currentTimeMillis()
    QuickSort.quickSort(array.clone())
    val t1 = System.currentTimeMillis()

    val t2 = System.currentTimeMillis()
    QuickSort.quickSortParallel(array.clone())
    val t3 = System.currentTimeMillis()

    println("Non Parallel QuickSort: " + (t1 - t0) + " [ms]")
    println("Parallel QuickSort: " + (t3 - t2) + " [ms]")
  }

  def binaryTreeTest(): Unit ={
    val t0 = System.currentTimeMillis()
    BinaryTree.generateTree(10, 0, 100)
    val t1 = System.currentTimeMillis()

    val t2 = System.currentTimeMillis()
    BinaryTree.generateTreeParallel(10, 0, 100)
    val t3 = System.currentTimeMillis()

    println("Non Parallel Binary Tree: " + (t1 - t0) + " [ms]")
    println("Parallel Binary Tree: " + (t3 - t2) + " [ms]")
  }

  def fibonacciTest(): Unit ={
    val n = 1000000000

    val t0 = System.currentTimeMillis()
    Fibonacci.fib(n)
    val t1 = System.currentTimeMillis()

    val t2 = System.currentTimeMillis()
    Fibonacci.fibParallel(n)
    val t3 = System.currentTimeMillis()

    println("Non Parallel Fibonacci: " + (t1 - t0) + " [ms]")
    println("Parallel Fibonacci: " + (t3 - t2) + " [ms]")
  }

  def mergeSortTest(): Unit = {
    @tailrec
    def generateRandomList(n: Int, newList: List[Int]): List[Int] = {
      if (n == 0) newList
      else generateRandomList(n - 1,Random.nextInt(10000)::newList)
    }
    val list = generateRandomList(1000000,Nil)

    val t0 = System.currentTimeMillis()
    MergeSort.mergeSort(list)
    val t1 = System.currentTimeMillis()

    val t2 = System.currentTimeMillis()
    MergeSort.mergeSortParallel(list)
    val t3 = System.currentTimeMillis()

    println("Non Parallel MergeSort: " + (t1 - t0) + " [ms]")
    println("Parallel MergeSort: " + (t3 - t2) + " [ms]")
  }
}
