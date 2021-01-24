import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object Test {
  def main(args: Array[String]): Unit = {
    quickSortTest()

    mergeSortTest()

    fibTest()
  }

  def quickSortTest(): Unit = {
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

  def fibTest(): Unit ={
    val n = 1000000000

    val t0 = System.currentTimeMillis()
    FibNum.fib(n)
    val t1 = System.currentTimeMillis()

    val t2 = System.currentTimeMillis()
    FibNum.fibParallel(n)
    val t3 = System.currentTimeMillis()

    println("Non Parallel Fibonacci: " + (t1 - t0) + " [ms]")
    println("Parallel Fibonacci: " + (t3 - t2) + " [ms]")
  }
}