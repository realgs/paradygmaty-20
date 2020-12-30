import org.scalatest.FunSuite
import List6.MergeSort
import List6.PrimeNumbers
import List6.SearchMaxTree
import List6.TreesToTest
import List6.SumOfAllPrevElem
import scala.util.Random
import org.scalameter._

class L6Test extends FunSuite {

  test("Test for merge sort: ") {
    assert(MergeSort.sort(List(4,0,9,-8,1,-7)) == List(-8,-7,0,1,4,9))
    assert(MergeSort.sort(List("a", "b", "adnotacja", "ada", "g", "buda")) == List("a", "ada", "adnotacja", "b", "buda", "g"))
    assert(MergeSort.sort(List(5.8, 0.9, -7.9, 5.8, 5.7, 9.8)) == List(-7.9, 0.9, 5.7, 5.8, 5.8, 9.8))
    assert(MergeSort.sort(List())(Ordering[String]) == List())
  }

  test("Test for merge sort with parallel way: ") {
    assert(MergeSort.parSort(List(4,0,9,-8,1,-7)) == List(-8,-7,0,1,4,9))
    assert(MergeSort.parSort(List("a", "b", "adnotacja", "ada", "g", "buda")) == List("a", "ada", "adnotacja", "b", "buda", "g"))
    assert(MergeSort.parSort(List(5.8, 0.9, -7.9, 5.8, 5.7, 9.8)) == List(-7.9, 0.9, 5.7, 5.8, 5.8, 9.8))
    assert(MergeSort.parSort(List())(Ordering[String]) == List())
  }

  test("Measuring time of mergeSort and comparison with parallel mergeSort implementation with Integer: ") {
    val r = new Random()
    val testList1 = List.fill(100)(r.nextInt(10000))
    val testList2 = List.fill(1000)(r.nextInt(100000))
    println("Testing list of 100 elements of random Integer: ")
    val mergeSortTime1 = measure {
      MergeSort.sort(testList1)
    }
    println("Time for usual merge sort: (100 elements) " + mergeSortTime1)
    val testListPar1 = testList1
    val parMergeSortTime1 = measure {
      MergeSort.parSort(testListPar1)
    }
    println("Time for parallel merge sort: (100 elements) " + parMergeSortTime1)
    val mergeSortTime2 = measure {
      MergeSort.sort(testList2)
    }
    println("Testing list of 1000 elements of random Integer: ")
    println("Time for usual merge sort: (1000 elements) " + mergeSortTime2)
    val testListPar2 = testList2
    val parMergeSortTime2 = measure {
      MergeSort.parSort(testListPar2)
    }
    println("Time for parallel merge sort: (1000 elements) " + parMergeSortTime2)
  }

  test("Measuring time of mergeSort and comparison with parallel mergeSort implementation with Double: ") {
    val r = new Random()
    val testList1 = List.fill(100)(r.nextDouble())
    val testList2 = List.fill(1000)(r.nextDouble())
    println("Testing list of 100 elements of random Double: ")
    val mergeSortTime1 = measure {
      MergeSort.sort(testList1)
    }
    println("Time for usual merge sort: (100 elements) " + mergeSortTime1)
    val testListPar1 = testList1
    val parMergeSortTime1 = measure {
      MergeSort.parSort(testListPar1)
    }
    println("Time for parallel merge sort: (100 elements) " + parMergeSortTime1)
    val mergeSortTime2 = measure {
      MergeSort.sort(testList2)
    }
    println("Testing list of 1000 elements of random Double: ")
    println("Time for usual merge sort: (1000 elements) " + mergeSortTime2)
    val testListPar2 = testList2
    val parMergeSortTime2 = measure {
      MergeSort.parSort(testListPar2)
    }
    println("Time for parallel merge sort: (1000 elements) " + parMergeSortTime2)
  }

  test("Measuring time of mergeSort and comparison with parallel mergeSort implementation with String: ") {
    val r = new Random()
    val testList1 = List.fill(100)(r.nextString(6))
    val testList2 = List.fill(1000)(r.nextString(10))
    println("Testing list of 100 elements of random String: ")
    val mergeSortTime1 = measure {
      MergeSort.sort(testList1)
    }
    println("Time for usual merge sort: (100 elements) " + mergeSortTime1)
    val testListPar1 = testList1
    val parMergeSortTime1 = measure {
      MergeSort.parSort(testListPar1)
    }
    println("Time for parallel merge sort: (100 elements) " + parMergeSortTime1)
    val mergeSortTime2 = measure {
      MergeSort.sort(testList2)
    }
    println("Testing list of 1000 elements of random String: ")
    println("Time for usual merge sort: (1000 elements) " + mergeSortTime2)
    val testListPar2 = testList2
    val parMergeSortTime2 = measure {
      MergeSort.parSort(testListPar2)
    }
    println("Time for parallel merge sort: (1000 elements) " + parMergeSortTime2)
  }

  test("Test of method, which search prime numbers: ") {
    assert(PrimeNumbers.ifPrime(2))
    assert(!PrimeNumbers.ifPrime(10))
    assert(PrimeNumbers.ifPrime(5))
    assert(!PrimeNumbers.ifPrime(12))
    assert(PrimeNumbers.searchPrime(10) == List(2,3,5,7))
    assert(PrimeNumbers.searchPrime(37) == List(2,3,5,7,11,13,17,19,23,29,31, 37))
    assert(PrimeNumbers.searchPrime(-10) == List())
    assert(PrimeNumbers.searchPrime(0) == List())
    assert(PrimeNumbers.searchPrime(1) == List())
    assert(PrimeNumbers.searchPrime(2) == List(2))
  }

  test("Test of parallel method, which search prime numbers: ") {
    assert(PrimeNumbers.ifPrime(2))
    assert(!PrimeNumbers.ifPrime(10))
    assert(PrimeNumbers.ifPrime(5))
    assert(!PrimeNumbers.ifPrime(12))
    assert(PrimeNumbers.parSearchPrime(10) == List(2,3,5,7))
    assert(PrimeNumbers.parSearchPrime(37) == List(2,3,5,7,11,13,17,19,23,29,31, 37))
    assert(PrimeNumbers.parSearchPrime(-10) == List())
    assert(PrimeNumbers.parSearchPrime(0) == List())
    assert(PrimeNumbers.parSearchPrime(1) == List())
    assert(PrimeNumbers.parSearchPrime(2) == List(2))
  }

  test("Measuring time of searchPrime and comparison with parallel searchPrime implementation with Integer: ") {
    println("Searching prime numbers from 100 elements: ")
    val searchPrimeTime1 = measure {
      PrimeNumbers.searchPrime(100)
    }
    println("Time for usual search prime: (100 elements) " + searchPrimeTime1)
    val parSearchPrimeTime1 = measure {
      PrimeNumbers.parSearchPrime(100)
    }
    println("Time for parallel search prime: (100 elements) " + parSearchPrimeTime1)
    val searchPrimeTime2 = measure {
      PrimeNumbers.searchPrime(1000)
    }
    println("Searching prime numbers from 1000 elements: ")
    println("Time for usual search prime: (1000 elements) " + searchPrimeTime2)
    val parSearchPrimeTime2 = measure {
      PrimeNumbers.parSearchPrime(1000)
    }
    println("Time for parallel search prime: (1000 elements) " + parSearchPrimeTime2)
    println("Searching prime numbers from 10000 elements: ")
    val searchPrimeTime3 = measure {
      PrimeNumbers.searchPrime(10000)
    }
    println("Time for usual search prime: (10000 elements) " + searchPrimeTime3)
    val parSearchPrimeTime3 = measure {
      PrimeNumbers.parSearchPrime(10000)
    }
    println("Time for parallel search prime: (10000 elements) " + parSearchPrimeTime3)
  }

  test("Test for searchMax method using tree data structure: ") {
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput1) == TreesToTest.max1)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput2) == TreesToTest.max2)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput3) == TreesToTest.max3)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput4) == TreesToTest.max4)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput5) == TreesToTest.max5)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput6) == TreesToTest.max6)
    assert(SearchMaxTree.searchMax(TreesToTest.treeInput7) == TreesToTest.max7)
  }

  test("Test for parallel searchMax method using tree data structure: ") {
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput1) == TreesToTest.max1)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput2) == TreesToTest.max2)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput3) == TreesToTest.max3)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput4) == TreesToTest.max4)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput5) == TreesToTest.max5)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput6) == TreesToTest.max6)
    assert(SearchMaxTree.parSearchMax(TreesToTest.treeInput7) == TreesToTest.max7)
  }

  test("Measuring time of search max in tree in comparison with parallel way: ") {
    println("First research: ")
    val time1 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput1)
    }
    println("Time for usual searching: " + time1)
    val parTime1 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput1)
    }
    println("Time for parallel searching: " + parTime1)
    println("Second research: ")
    val time2 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput2)
    }
    println("Time for usual searching: " + time2)
    val parTime2 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput2)
    }
    println("Time for parallel searching: " + parTime2)
    println("Third research: ")
    val time3 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput3)
    }
    println("Time for usual searching: " + time3)
    val parTime3 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput3)
    }
    println("Time for parallel searching: " + parTime3)
    println("Fourth research: ")
    val time4 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput4)
    }
    println("Time for usual searching: " + time4)
    val parTime4 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput4)
    }
    println("Time for parallel searching: " + parTime4)
    println("Fifth research: ")
    val time5 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput5)
    }
    println("Time for usual searching: " + time5)
    val parTime5 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput5)
    }
    println("Time for parallel searching: " + parTime5)
    println("Sixth research: ")
    val time6 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput6)
    }
    println("Time for usual searching: " + time6)
    val parTime6 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput6)
    }
    println("Time for parallel searching: " + parTime6)
    println("Seventh research: ")
    val time7 = measure {
      SearchMaxTree.searchMax(TreesToTest.treeInput7)
    }
    println("Time for usual searching: " + time7)
    val parTime7 = measure {
      SearchMaxTree.parSearchMax(TreesToTest.treeInput7)
    }
    println("Time for parallel searching: " + parTime7)
  }

  test("Measuring time of search max in random tree in comparison with parallel way: ") {
    println("First research: ")
    val tree1 = TreesToTest.createTree(5, 0, 1000)
    val time1 = measure {
      SearchMaxTree.searchMax(tree1)
    }
    println("Time for usual searching: " + time1)
    val parTree1 = tree1
    val parTime1 = measure {
      SearchMaxTree.parSearchMax(parTree1)
    }
    println("Time for parallel searching: " + parTime1)
    println("Second research: ")
    val tree2 = TreesToTest.createTree(10, 0, 10000)
    val time2 = measure {
      SearchMaxTree.searchMax(tree2)
    }
    println("Time for usual searching: " + time2)
    val parTree2 = tree2
    val parTime2 = measure {
      SearchMaxTree.parSearchMax(parTree2)
    }
    println("Time for parallel searching: " + parTime2)
    val tree3 = TreesToTest.createTree(15, -1000, 100000)
    println("Third research: ")
    val time3 = measure {
      SearchMaxTree.searchMax(tree3)
    }
    println("Time for usual searching: " + time3)
    val parTree3 = tree3
    val parTime3 = measure {
      SearchMaxTree.parSearchMax(parTree3)
    }
    println("Time for parallel searching: " + parTime3)
  }

  test("Test for sumOfAllPrevElem method: ") {
    assert(SumOfAllPrevElem.sumOfAllPrevElem(Array(1,2,3,4,5,6,7)) == 28)
    assert(SumOfAllPrevElem.sumOfAllPrevElem(Array(1)) == 1)
    assert(SumOfAllPrevElem.sumOfAllPrevElem(Array(-1,2,-3,4,-5,6,-7)) == -4)
    assert(SumOfAllPrevElem.sumOfAllPrevElem(Array(-1,-2,-3,-4,-5,-6,-7)) == -28)
    assert(SumOfAllPrevElem.sumOfAllPrevElem(Array()) == 0)
  }

  test("Test for parallel sumOfAllPrevElem method: ") {
    assert(SumOfAllPrevElem.parSumOfAllPrevElem(Array(1,2,3,4,5,6,7)) == 28)
    assert(SumOfAllPrevElem.parSumOfAllPrevElem(Array(1)) == 1)
    assert(SumOfAllPrevElem.parSumOfAllPrevElem(Array(-1,2,-3,4,-5,6,-7)) == -4)
    assert(SumOfAllPrevElem.parSumOfAllPrevElem(Array(-1,-2,-3,-4,-5,-6,-7)) == -28)
    assert(SumOfAllPrevElem.parSumOfAllPrevElem(Array()) == 0)
  }

  test("Measuring time for suming all prevoius elements in array in comparison with parallel way: ") {
    val r = new Random()
    val testArray1 = Array.fill(100)(r.nextInt(6000000))
    val testArray2 = Array.fill(1000)(r.nextInt(1000000000))
    val testArray3 = Array.fill(10000)(r.nextInt(1000000000))
    println("Testing array of 100 elements of random Integers: ")
    val time1 = measure {
      SumOfAllPrevElem.sumOfAllPrevElem(testArray1)
    }
    println("Time for usual sum method: (100 elements) " + time1)
    val testArrayPar1 = testArray1
    val parTime1 = measure {
      SumOfAllPrevElem.parSumOfAllPrevElem(testArrayPar1)
    }
    println("Time for parallel sum method: (100 elements) " + parTime1)
    val time2 = measure {
      SumOfAllPrevElem.sumOfAllPrevElem(testArray2)
    }
    println("Testing array of 1000 elements of random Integers: ")
    println("Time for usual sum method: (1000 elements) " + time2)
    val testArrayPar2 = testArray2
    val parTime2 = measure {
      SumOfAllPrevElem.sumOfAllPrevElem(testArrayPar2)
    }
    println("Time for parallel sum method: (1000 elements) " + parTime2)
    println("Testing array of 10000 elements of random Integers: ")
    val time3 = measure {
      SumOfAllPrevElem.sumOfAllPrevElem(testArray3)
    }
    println("Time for usual sum method: (10000 elements) " + time3)
    val testArrayPar3 = testArray3
    val parTime3 = measure {
      SumOfAllPrevElem.parSumOfAllPrevElem(testArrayPar3)
    }
    println("Time for parallel sum method: (10000 elements) " + parTime3)
  }

  /*Conclusion:
  Parallel computing can be very effective especially when our input data have a lot of elements(often much bigger than 100)
  Some algorithms with almost 100 elements or less need to even more time to compute with parallel computing in comparison
  with sequential computing. */

}
