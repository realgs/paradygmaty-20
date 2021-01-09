import Benchmarking._
import Quicksort._
import Mergesort._
import MatrixMultiplication._
import Trees._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    quicksortTests(100000, 100)
    println()
    mergesortTests(2000, 100)
    println()
    treeTraversingTests(4,2, 100)
    println()
    matrixMultiplicationTests(200, 100)
  }
  def quicksortTests(arraySize: Int, numberOfTestSets: Int, parallelismLevel: Int = Runtime.getRuntime.availableProcessors(), repeatsPerTestSet: Int = 1): Unit = {
    def quicksortBenchmark(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats] = compareBenchmarkArray(List(quicksort _, quicksortParallel(parallelismLevel) _))((n: Int) => Array.fill(n)(Random.nextInt()))(arraySize)(numberOfTestSets)(repeatsPertestSet)
    val result = quicksortBenchmark(numberOfTestSets)(repeatsPerTestSet).flatten
    println(s"quicksort sequential: ${result(0).mean}")
    println(s"quicksort parallel: ${result(1).mean}")
    println(s"parallel ${result(0).mean / result(1).mean} times quicker")
  }

  def mergesortTests(listSize: Int, numberOfTestSets: Int, parallelismLevel: Int = Runtime.getRuntime.availableProcessors(), repeatsPerTestSet: Int = 1): Unit = {
    def mergesortBenchmark(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats] = compareBenchmark(List(mergeSort((x: Int) => (y: Int) => x < y) _, mergeSortParallel((x: Int) => (y: Int) => x < y)(8) _))((n: Int) => List.fill(n)(Random.nextInt()))(listSize)(numberOfTestSets)(repeatsPertestSet)
    val result = mergesortBenchmark(numberOfTestSets)(repeatsPerTestSet).flatten
    println(s"mergesort sequential: ${result(0).mean}")
    println(s"mergesort parallel: ${result(1).mean}")
    println(s"parallel ${result(0).mean / result(1).mean} times quicker")
  }

  def matrixMultiplicationTests(matrixesSize: Int, numberOfTestSets: Int, parallelismLevel: Int = Runtime.getRuntime.availableProcessors(), repeatsPerTestSet: Int = 1): Unit = {
    def matrixBenchmark(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats] = compareBenchmark2Arrays(List(matrixMultiplication _, matrixMultiplicationParallel(8) _))((n: Int) => (Array.fill(n, n)(Random.nextInt()), Array.fill(n, n)(Random.nextInt())))(matrixesSize)(numberOfTestSets)(repeatsPertestSet)
    val result = matrixBenchmark(numberOfTestSets)(repeatsPerTestSet).flatten
    println(s"matrix multiplication sequential: ${result(0).mean}")
    println(s"matrix multiplication parallel: ${result(1).mean}")
    println(s"parallel ${result(0).mean / result(1).mean} times quicker")
  }

  def treeTraversingTests(treeDepth: Int, treeWidth: Int, numberOfTestSets: Int, repeatsPerTestSet: Int = 1): Unit = {
    def treesBenchmark(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats] = compareBenchmark(List( treeLeavesMin _, treeLeavesMinParallel _))((params: (Int, Int)) => generateRegularTree(params._1, params._2))(treeDepth,treeWidth)(numberOfTestSets)(repeatsPertestSet)
    val result = treesBenchmark(numberOfTestSets)(repeatsPerTestSet).flatten
    println(s"tree traversing sequential: ${result(0).mean}")
    println(s"tree traversing parallel: ${result(1).mean}")
    println(s"parallel ${result(0).mean / result(1).mean} times quicker")
  }
}
