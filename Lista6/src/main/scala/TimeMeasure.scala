
import org.scalameter.{Quantity, measure}

import scala.util.Random

object TimeMeasure {

  def main(args: Array[String]): Unit ={
    val ran = new Random()

    val warmupArray: Array[Int] = Array.fill(10000)(ran.nextInt(10000))
    val array10: Array[Int] = Array.fill(10)(ran.nextInt(10000))
    val array100: Array[Int] = Array.fill(100)(ran.nextInt(10000))
    val array1000: Array[Int] = Array.fill(1000)(ran.nextInt(10000))
    val array10000: Array[Int] = Array.fill(10000)(ran.nextInt(10000))
    val array50000: Array[Int] = Array.fill(50000)(ran.nextInt(10000))

    //sequential
    val sortWarmupTime = measure {
      MergeSort.mergeSort(warmupArray.clone())
    }
    val array10SortingTime =  measure {
      MergeSort.mergeSort(array10.clone())
    }
    val array100SortingTime =  measure {
      MergeSort.mergeSort(array100.clone())
    }
    val array1000SortingTime =  measure {
      MergeSort.mergeSort(array1000.clone())
    }
    val array10000SortingTime =  measure {
      MergeSort.mergeSort(array10000.clone())
    }
    val array50000SortingTime = measure {
      MergeSort.mergeSort(array50000.clone())
    }

    //parallel
    val parallelSortWarmupTime = measure {
      MergeSort.parallelMergeSort(warmupArray)
    }
    val array10ParallelSortingTime =  measure {
      MergeSort.parallelMergeSort(array10)
    }
    val array100ParallelSortingTime =  measure {
      MergeSort.parallelMergeSort(array100)
    }
    val array1000ParallelSortingTime =  measure {
      MergeSort.parallelMergeSort(array1000)
    }
    val array10000ParallelSortingTime =  measure {
      MergeSort.parallelMergeSort(array10000)
    }
    val array50000ParallelSortingTime = measure {
      MergeSort.parallelMergeSort(array50000)
    }

    println("***** MergeSort *****")
    println("size | sequential | parallel")
    println(s"warmup (10000) | $sortWarmupTime | $parallelSortWarmupTime")
    println(s"10 | $array10SortingTime | $array10ParallelSortingTime")
    println(s"100 | $array100SortingTime | $array100ParallelSortingTime")
    println(s"1000 | $array1000SortingTime | $array1000ParallelSortingTime")
    println(s"10000 | $array10000SortingTime | $array10000ParallelSortingTime")
    println(s"50000 | $array50000SortingTime | $array50000ParallelSortingTime")

    //sequential
    val sumWarmupTime = measure {
      ArraySum.sum(warmupArray)
    }
    val array10SumTime =  measure {
      ArraySum.sum(array10)
    }
    val array100SumTime: Quantity[Double] = measure {
      ArraySum.sum(array100)
    }
    val array1000SumTime: Quantity[Double] = measure {
      ArraySum.sum(array1000)
    }
    val array10000SumTime: Quantity[Double] = measure {
      ArraySum.sum(array10000)
    }
    val array50000SumTime: Quantity[Double] = measure {
      ArraySum.sum(array50000)
    }

    //parallel
    val parallelSumWarmupTime = measure {
      ArraySum.parallelSum(warmupArray)
    }
    val array10ParallelSumTime: Quantity[Double] = measure {
      ArraySum.parallelSum(array10)
    }
    val array100ParallelSumTime: Quantity[Double] = measure {
      ArraySum.parallelSum(array100)
    }
    val array1000ParallelSumTime: Quantity[Double] = measure {
      ArraySum.parallelSum(array1000)
    }
    val array10000ParallelSumTime: Quantity[Double] =  measure {
      ArraySum.parallelSum(array10000)
    }
    val array50000ParallelSumTime: Quantity[Double] = measure {
      ArraySum.parallelSum(array50000)
    }

    println("\n***** ArraySum *****")
    println("size | sequential | parallel")
    println(s"warmup (10000) | $sumWarmupTime | $parallelSumWarmupTime")
    println(s"10 | $array10SumTime | $array10ParallelSumTime")
    println(s"100 | $array100SumTime | $array100ParallelSumTime")
    println(s"1000 | $array1000SumTime | $array1000ParallelSumTime")
    println(s"10000 | $array10000SumTime | $array10000ParallelSumTime")
    println(s"50000 | $array50000SumTime | $array50000ParallelSumTime")

    val warmupTree = Trees.generateTree(3, 0, 100)
    val tree2Levels = Trees.generateTree(2,0,100)
    val tree5Levels = Trees.generateTree(5,0,100)
    val tree10Levels = Trees.generateTree(10,0,100)
    val tree15Levels = Trees.generateTree(15,0,100)

    //sequential
    val treeToListWarmupTime = measure {
      Trees.treeToList(warmupTree)
    }
    val tree2ToListTime = measure {
      Trees.treeToList(tree2Levels)
    }
    val tree5ToListTime = measure {
      Trees.treeToList(tree5Levels)
    }
    val tree10ToListTime = measure {
      Trees.treeToList(tree10Levels)
    }
    val tree11ToListTime = measure {
      Trees.treeToList(tree15Levels)
    }

    //parallel
    val treeToListParallelWarmupTime = measure {
      Trees.treeToList(warmupTree)
    }
    val tree2ToListParallelTime = measure {
      Trees.treeToList(tree2Levels)
    }
    val tree5ToListParallelTime = measure {
      Trees.treeToList(tree5Levels)
    }
    val tree10ToListParallelTime = measure {
      Trees.treeToList(tree10Levels)
    }
    val tree11ToListParallelTime = measure {
      Trees.treeToList(tree15Levels)
    }

    println("\n***** TreeToList *****")
    println("height | sequential | parallel")
    println(s"warmup (3) | $treeToListWarmupTime | $treeToListParallelWarmupTime")
    println(s"2 | $tree2ToListTime | $tree2ToListParallelTime")
    println(s"5 | $tree5ToListTime | $tree5ToListParallelTime")
    println(s"10 | $tree10ToListTime | $tree10ToListParallelTime")
    println(s"15 | $tree11ToListTime | $tree11ToListParallelTime")

  }
}
