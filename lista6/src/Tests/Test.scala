package Tests

import Tasks.Quicksort.{quicksort, quicksortParallel}
import Tasks.Primes.{generatePrimes, generatePrimesParallel}
import Tasks.Tree.{areTreesTheSame, areTreesTheSameParallel, generateTree, copyTree, Empty, Node}
import Tasks.FibonacciNum.{fib, fibParallel}

import scala.util.Random

object Test {

  def runTests(): Unit = {
    quicksortTest()
    primesTest()
    fibonacciNumTest()
    areTreesTheSameTest()
  }

  def quicksortTest(): Unit = {
    println("***Quicksort test***")
    for (i <- 1 to 5) {
      val array1 = Array.fill(100)(Random.nextInt(1000))
      val array2 = array1.clone()
      var array3 = array1.clone()
      quicksort(array1)
      quicksortParallel(array2)
      array3 = array3.sorted
      println(i + ". try: " + (array1 sameElements array2) + ", " + (array2 sameElements array3))
    }
  }

  def primesTest(): Unit = {
    println("\n***Primes generator test***")
    var primes = generatePrimes(90)
    var primes1 = generatePrimesParallel(90)
    var realPrimes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89)
    println((primes == primes1) + ", " + (primes == realPrimes))

    primes = generatePrimes(190)
    primes1 = generatePrimesParallel(190)
    realPrimes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181)
    println((primes == primes1) + ", " + (primes == realPrimes))
  }

  def areTreesTheSameTest(): Unit = {
    println("\n***Are trees the same test***")
    val tree = Node(Node(Node(1, Node(2, Empty, Empty), Node(10, Empty, Empty)), Empty, Empty), Node(11, Empty, Empty), Empty)
    val tree1 = copyTree(tree)
    println("expected: true; actual: " + areTreesTheSame(tree, tree1) + ", " + areTreesTheSameParallel(tree, tree1))
    println("expected: true; actual: " + areTreesTheSame(tree, Node(Node(Node(1, Node(2, Empty, Empty), Node(10, Empty, Empty)), Empty, Empty), Node(11, Empty, Empty), Empty))
      + ", " + areTreesTheSameParallel(tree, Node(Node(Node(1, Node(2, Empty, Empty), Node(10, Empty, Empty)), Empty, Empty), Node(11, Empty, Empty), Empty)))

    val tree2 = generateTree(10, 10, 10000)
    val tree3 = copyTree(tree2)
    println("expected: true; actual: " + areTreesTheSame(tree2, tree3) + ", " + areTreesTheSameParallel(tree2, tree3))
    println("expected: false; actual: " + areTreesTheSame(tree2, tree1) + ", " + areTreesTheSameParallel(tree2, tree1))

  }

  def fibonacciNumTest(): Unit = {
    println("\n***Fibonacci number test***")
    var f1 = fib(42)
    var f2 = fibParallel(42)
    println((f1 == f2) + ", " + (f1 == 433494437))

    f1 = fib(25)
    f2 = fibParallel(25)
    println((f1 == f2) + ", " + (f1 == 121393))
  }
}
