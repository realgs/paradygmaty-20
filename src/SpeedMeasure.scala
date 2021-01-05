import org.scalameter._

import scala.util.Random
import QuickSort._
import ArraySum._
import MergeSort._

object SpeedMeasure extends App {
  val array10Size = Array.fill(10)(new Random().nextInt(1000))
  val array100Size = Array.fill(100)(new Random().nextInt(1000))
  val array1000Size = Array.fill(1000)(new Random().nextInt(1000))
  val array10000Size = Array.fill(10000)(new Random().nextInt(1000))
  val array100000Size = Array.fill(100000)(new Random().nextInt(1000))
  val array1000000Size = Array.fill(1000000)(new Random().nextInt(1000))
  val array10000000Size = Array.fill(10000000)(new Random().nextInt(1000))
  val array100000000Size = Array.fill(100000000)(new Random().nextInt(1000))

  val timeToSortArray10Size = measure {
    quickSort(array10Size)
  }
  val timeToSortArray100Size = measure {
    quickSort(array100Size)
  }
  val timeToSortArray1000Size = measure {
    quickSort(array1000Size)
  }
  val timeToSortArray10000Size = measure {
    quickSort(array10000Size)
  }
  val timeToSortArray100000Size = measure {
    quickSort(array100000Size)
  }
  val timeToSortArray1000000Size = measure {
    quickSort(array1000000Size)
  }
  val timeToSortArray10000000Size = measure {
    quickSort(array10000000Size)
  }

  val timeToParallelSortArray10Size = measure {
    quickSortParallel(array10Size)
  }
  val timeToParallelSortArray100Size = measure {
    quickSortParallel(array100Size)
  }
  val timeToParallelSortArray1000Size = measure {
    quickSortParallel(array1000Size)
  }
  val timeToParallelSortArray10000Size = measure {
    quickSortParallel(array10000Size)
  }
  val timeToParallelSortArray100000Size = measure {
    quickSortParallel(array100000Size)
  }
  val timeToParallelSortArray1000000Size = measure {
    quickSortParallel(array1000000Size)
  }
  val timeToParallelSortArray10000000Size = measure {
    quickSortParallel(array10000000Size)
  }

  println("QUICKSORT")
  println("size | normal | parallel")
  println(s"10 | $timeToSortArray10Size | $timeToParallelSortArray10Size")
  println(s"100 | $timeToSortArray100Size | $timeToParallelSortArray100Size")
  println(s"1000 | $timeToSortArray1000Size | $timeToParallelSortArray1000Size")
  println(s"10000 | $timeToSortArray10000Size | $timeToParallelSortArray10000Size")
  println(s"100000 | $timeToSortArray100000Size | $timeToParallelSortArray100000Size")
  println(s"1000000 | $timeToSortArray1000000Size | $timeToParallelSortArray1000000Size")
  println(s"10000000 | $timeToSortArray10000000Size | $timeToParallelSortArray10000000Size")

  val timeToSumArray10Size = measure {
    calculateSum(array10Size)
  }
  val timeToSumArray100Size = measure {
    calculateSum(array100Size)
  }
  val timeToSumArray1000Size = measure {
    calculateSum(array1000Size)
  }
  val timeToSumArray10000Size = measure {
    calculateSum(array10000Size)
  }
  val timeToSumArray100000Size = measure {
    calculateSum(array100000Size)
  }
  val timeToSumArray1000000Size = measure {
    calculateSum(array1000000Size)
  }
  val timeToSumArray10000000Size = measure {
    calculateSum(array10000000Size)
  }
  val timeToSumArray100000000Size = measure {
    calculateSum(array100000000Size)
  }

  parCalculateSum(array10Size)
  val timeToParallelSumArray10Size = measure {
    parCalculateSum(array10Size)
  }
  val timeToParallelSumArray100Size = measure {
    parCalculateSum(array100Size)
  }
  val timeToParallelSumArray1000Size = measure {
    parCalculateSum(array1000Size)
  }
  val timeToParallelSumArray10000Size = measure {
    parCalculateSum(array10000Size)
  }
  val timeToParallelSumArray100000Size = measure {
    parCalculateSum(array100000Size)
  }
  val timeToParallelSumArray1000000Size = measure {
    parCalculateSum(array1000000Size)
  }
  val timeToParallelSumArray10000000Size = measure {
    parCalculateSum(array10000000Size)
  }
  val timeToParallelSumArray100000000Size = measure {
    parCalculateSum(array100000000Size)
  }

  println("ARRAY SUM")
  println("size | normal | parallel")
  println(s"10 | $timeToSumArray10Size | $timeToParallelSumArray10Size")
  println(s"100 | $timeToSumArray100Size | $timeToParallelSumArray100Size")
  println(s"1000 | $timeToSumArray1000Size | $timeToParallelSumArray1000Size")
  println(s"10000 | $timeToSumArray10000Size | $timeToParallelSumArray10000Size")
  println(s"100000 | $timeToSumArray100000Size | $timeToParallelSumArray100000Size")
  println(s"1000000 | $timeToSumArray1000000Size | $timeToParallelSumArray1000000Size")
  println(s"10000000 | $timeToSumArray10000000Size | $timeToParallelSumArray10000000Size")
  println(s"100000000 | $timeToSumArray100000000Size | $timeToParallelSumArray100000000Size")

  val array10SizeCopy = Array.copyOf(array10Size, array10Size.length)
  val array100SizeCopy = Array.copyOf(array100Size, array100Size.length)
  val array1000SizeCopy = Array.copyOf(array1000Size, array1000Size.length)
  val array10000SizeCopy = Array.copyOf(array10000Size, array10000Size.length)
  val array100000SizeCopy = Array.copyOf(array100000Size, array100000Size.length)
  val array1000000SizeCopy = Array.copyOf(array1000000Size, array1000000Size.length)
  val array10000000SizeCopy = Array.copyOf(array10000000Size, array10000000Size.length)

  sortWithMergeSort(array10Size)
  val timeToMergeSortArray10Size = measure {
    sortWithMergeSort(array10Size)
  }
  val timeToMergeSortArray100Size = measure {
    sortWithMergeSort(array100Size)
  }
  val timeToMergeSortArray1000Size = measure {
    sortWithMergeSort(array1000Size)
  }
  val timeToMergeSortArray10000Size = measure {
    sortWithMergeSort(array10000Size)
  }
  val timeToMergeSortArray100000Size = measure {
    sortWithMergeSort(array100000Size)
  }
  val timeToMergeSortArray1000000Size = measure {
    sortWithMergeSort(array1000000Size)
  }
  val timeToMergeSortArray10000000Size = measure {
    sortWithMergeSort(array10000000Size)
  }

  val timeToParallelMergeSortArray10Size = measure {
    sortWithMergeSortParallel(array10SizeCopy)
  }
  val timeToParallelMergeSortArray100Size = measure {
    sortWithMergeSortParallel(array100SizeCopy)
  }
  val timeToParallelMergeSortArray1000Size = measure {
    sortWithMergeSortParallel(array1000SizeCopy)
  }
  val timeToParallelMergeSortArray10000Size = measure {
    sortWithMergeSortParallel(array10000SizeCopy)
  }
  val timeToParallelMergeSortArray100000Size = measure {
    sortWithMergeSortParallel(array100000SizeCopy)
  }
  val timeToParallelMergeSortArray1000000Size = measure {
    sortWithMergeSortParallel(array1000000SizeCopy)
  }
  val timeToParallelMergeSortArray10000000Size = measure {
    sortWithMergeSortParallel(array10000000SizeCopy)
  }

  println("MERGESORT")
  println("size | normal | parallel")
  println(s"10 | $timeToMergeSortArray10Size | $timeToParallelMergeSortArray10Size")
  println(s"100 | $timeToMergeSortArray100Size | $timeToParallelMergeSortArray100Size")
  println(s"1000 | $timeToMergeSortArray1000Size | $timeToParallelMergeSortArray1000Size")
  println(s"10000 | $timeToMergeSortArray10000Size | $timeToParallelMergeSortArray10000Size")
  println(s"100000 | $timeToMergeSortArray100000Size | $timeToParallelMergeSortArray100000Size")
  println(s"1000000 | $timeToMergeSortArray1000000Size | $timeToParallelMergeSortArray1000000Size")
  println(s"10000000 | $timeToMergeSortArray10000000Size | $timeToParallelMergeSortArray10000000Size")
}
