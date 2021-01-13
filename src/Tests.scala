import Fibonacci.{fib, fibFuture, fibParallel}
import HttpRequest.{getData, getDataFuture, getDataParallel}
import ParallelMechanism.parallel
import PreorderTree.{generateTree, preorderTraversal, preorderTraversalFuture, preorderTraversalParallel}
import Quicksort.{quick, quickFuture, quickParallel, quicksort}
import UsageMeasurer.{measureTime, measureTime2}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
object Tests {
  def main(args: Array[String]): Unit = {
    // Time test
    timeTest()
    // Tree preorder traversal
    treePreorderTraversal()
    // HTTP GET request
    httpRequestTest()
    // Quicksort
    quicksortTest()
    // Fibonacci
    fibTest()
  }

  def timeTest(): Unit = {
    def test(): Unit = Thread.sleep(100)

    println("Total time: " + measureTime(test(), test(), 10, showDetails = true))
    // Test for 2 simultaneously performed tasks
    println("Total time: " + measureTime(parallel(test(), test()), (), 10, showDetails = true))
    println("Total time: " + measureTime(Future(test()), Future(test()), 10, showDetails = true))

    println("Total time: " + measureTime2(test(), test(), test(), test(), 10, showDetails = true))
    // Test for 4 simultaneously performed tasks
    println("Total time: " + measureTime2(parallel(test(), test(), test(), test()), (), (), (),  10, showDetails = true))
    println("Total time: " + measureTime2(Future(test()), Future(test()), Future(test()), Future(test()), 10, showDetails = true))
  }

  def treePreorderTraversal(): Unit = {
    val t = generateTree(3, 1, 30)
    // println(t)
    println("Total time: " + measureTime(preorderTraversal(t), preorderTraversal(t), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalParallel(t), (), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalFuture(t), (), 10, showDetails = false))
    // println(preorderTraversal(t))

    val t2 = generateTree(14, 1, 5)
    println("Total time: " + measureTime(preorderTraversal(t2), preorderTraversal(t2), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalParallel(t2), (), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalFuture(t2), (), 10, showDetails = false))

    val t3 = generateTree(20, 1, 5)
    println("Total time: " + measureTime(preorderTraversal(t3), preorderTraversal(t3), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalParallel(t3), (), 10, showDetails = false))
    println("Total time: " + measureTime(preorderTraversalFuture(t3), (), 10, showDetails = false))
    // The bigger the tree (+- n >= 18), the more effective preorder using Parallel or Future is.
  }

  def httpRequestTest(): Unit = {
    def getPosts: (String, String) = getData("https://jsonplaceholder.typicode.com/users/1", "https://jsonplaceholder.typicode.com/users/2")
    def getPostsParallel: (String, String) = getDataParallel("https://jsonplaceholder.typicode.com/users/1", "https://jsonplaceholder.typicode.com/users/2")
    def getPostsFuture: (String, String) = getDataFuture("https://jsonplaceholder.typicode.com/users/1", "https://jsonplaceholder.typicode.com/users/2")

    val getPostsResult = measureTime(getPosts, (), 20, showDetails = false)
    val getPostsParallelResult = measureTime(getPostsParallel, (), 20, showDetails = false)
    val getPostsFutureResult = measureTime(getPostsFuture, (), 20, showDetails = false)

//    println(getPosts)
//    println(getPostsParallel)
//    println(getPostsFuture)

    println("Total time: " + getPostsResult)
    println("Total time: " + getPostsParallelResult)
    println("Total time: " + getPostsFutureResult)

    // Total time: 70.39679450000001 ms
    // Total time: 39.280837250000005 ms
    // Total time: 40.026436600000004 ms

    def getPhotos: (String, String) = getData("https://jsonplaceholder.typicode.com/photos", "")
    def getPhotosParallel: (String, String) = getDataParallel("https://jsonplaceholder.typicode.com/photos", "")
    def getPhotosFuture: (String, String) = getDataFuture("https://jsonplaceholder.typicode.com/photos", "")

    val getPhotosResult = measureTime(getPhotos, (), 20, showDetails = false)
    val getPhotosParallelResult = measureTime(getPhotosParallel, (), 20, showDetails = false)
    val getPhotosFutureResult = measureTime(getPhotosFuture, (), 20, showDetails = false)

//    println(getPhotos)
//    println(getPhotosParallel)
//    println(getPhotosFuture)

    println("Total time: " + getPhotosResult)
    println("Total time: " + getPhotosParallelResult)
    println("Total time: " + getPhotosFutureResult)

    // Total time: 154.2477597 ms
    // Total time: 83.78588320000001 ms
    // Total time: 74.397848 ms
  }

  def quicksortTest(): Unit = {
    val test = Array.fill(100000)(Random.between(0, 10000))

    println("Total time: " + measureTime(quicksort(quick, test.clone()), (), 20, showDetails = false))
    println("Total time: " + measureTime(quicksort(quickParallel, test.clone()), (), 20, showDetails = false))
    println("Total time: " + measureTime(quicksort(quickFuture, test), (), 20, showDetails = false))
//    println(test.toList)

//    10.000 elements [0-10000]
//    Total time: 2.6082249500000003 ms
//    Total time: 4.02530245 ms
//    Total time: 0.8700941 ms

//    100.000 elements [0-10000]
//    Total time: 22.800668 ms
//    Total time: 12.35783005 ms
//    Total time: 4.73374285 ms

//    1.000.000 elements [0-10000]
//    Total time: 151.50320095 ms
//    Total time: 114.2687592 ms
//    Total time: 51.67331125 ms

//    1) 10.000.000 elements [0-10000]
//    Total time: 1638.9212707499998 ms
//    Total time: 1491.00554075 ms
//    Total time: 630.8461628499998 ms

//    2) 10.000.000 elements [0-10000]
//    Total time: 1743.4961217999996 ms
//    Total time: 1326.5971557999999 ms
//    Total time: 905.2194478499999 ms
  }

  def fibTest(): Unit = {
    val n = 25
    println("Total time: " + measureTime(fib(n), (), 20, showDetails = false))
    println("Total time: " + measureTime(fibParallel(n), (),20, showDetails = false))
    println("Total time: " + measureTime(fibFuture(n), (), 20, showDetails = false))

//    n = 5; 100 runs
//    Total time: 0.023699690000000002 ms
//    Total time: 0.32674445999999996 ms
//    Total time: 0.6320909 ms

//    n = 20; 20 runs
//    Total time: 1.0481614000000001 ms
//    Total time: 0.93084645 ms
//    Total time: 0.8215877 ms

//    n = 30; 20 runs
//    Total time: 30.554102699999998 ms
//    Total time: 21.86102355 ms
//    Total time: 22.22651065 ms

//    n = 35; 50 runs
//    Total time: 354.07328265999996 ms
//    Total time: 285.75949122000003 ms
//    Total time: 282.2006281 ms

//    n = 40; 5 runs
//    Total time: 3430.0006576 ms
//    Total time: 2623.3623304 ms
//    Total time: 2673.7938248000005 ms

//    Comparison: fib vs fibTail
//    n = 40; 5 runs
//    Total time: 3896.8854027999996 ms - fib
//    Total time: 0.0097982 ms - fibTail
  }
}

