import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Random, Sorting}

class Tests extends AnyFunSuite{

  val quickS: Quicksort.type = Quicksort
  val mergeS: MergeSort.type = MergeSort
  val fibNumb: FibonacciNumb.type = FibonacciNumb

  test ("Test quick sorts 1000") {
    val ar1 = Array.fill(1000)(Random.nextInt())
    val ar1clone = ar1.clone()
    Sorting.quickSort(ar1clone)
    val ar2 = Array.fill(1000)(Random.nextInt())
    val ar2clone = ar2.clone()
    Sorting.quickSort(ar2clone)

    val start = System.currentTimeMillis
    quickS.quicksort(ar1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("QuickSort time for 1000 ints:\t" + res)

    quickS.quicksortParallel(ar2)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelQuickSort time for 1000 ints:\t" + res)

    assert(ar1 sameElements ar1clone)
    assert(ar2 sameElements ar2clone)
  }

  test ("Test quick sorts 1000000") {
    val ar1 = Array.fill(1000000)(Random.nextInt())
    val ar1clone = ar1.clone()
    Sorting.quickSort(ar1clone)
    val ar2 = Array.fill(1000000)(Random.nextInt())
    val ar2clone = ar2.clone()
    Sorting.quickSort(ar2clone)

    val start = System.currentTimeMillis
    quickS.quicksort(ar1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("QuickSort time for 1000000 ints:\t" + res)

    quickS.quicksortParallel(ar2)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelQuickSort time for 1000000 ints:\t" + res)

    assert(ar1 sameElements ar1clone)
    assert(ar2 sameElements ar2clone)
  }

  test ("Test quick sorts 10000000") {
    val ar1 = Array.fill(10000000)(Random.nextInt())
    val ar1clone = ar1.clone()
    Sorting.quickSort(ar1clone)
    val ar2 = Array.fill(10000000)(Random.nextInt())
    val ar2clone = ar2.clone()
    Sorting.quickSort(ar2clone)

    val start = System.currentTimeMillis
    quickS.quicksort(ar1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("QuickSort time for 10000000 ints:\t" + res)

    quickS.quicksortParallel(ar2)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelQuickSort time for 10000000 ints:\t" + res)

    assert(ar1 sameElements ar1clone)
    assert(ar2 sameElements ar2clone)
  }

  test ("Test merge sorts 100") {
    var list1 = List.fill(100)(Random.nextInt())
    var list2 = List.fill(100)(Random.nextInt())

    val start = System.currentTimeMillis
    list1 = mergeS.mergeSort(list1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("MergeSort time for 100 ints:\t" + res)

    list2 = mergeS.mergeSortParallel(list2, 4)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelMergeSort time for 100 ints:\t" + res)

    assert(mergeS.isSorted(list1))
    assert(mergeS.isSorted(list2))
  }

  test ("Test merge sorts 1000") {
    var list1 = List.fill(1000)(Random.nextInt())
    var list2 = List.fill(1000)(Random.nextInt())

    val start = System.currentTimeMillis
    list1 = mergeS.mergeSort(list1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("MergeSort time for 1000 ints:\t" + res)

    list2 = mergeS.mergeSortParallel(list2, 4)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelMergeSort time for 1000 ints:\t" + res)

    assert(mergeS.isSorted(list1))
    assert(mergeS.isSorted(list2))
  }

  test ("Test merge sorts 10000") {
    var list1 = List.fill(10000)(Random.nextInt())
    var list2 = List.fill(10000)(Random.nextInt())

    val start = System.currentTimeMillis
    list1 = mergeS.mergeSort(list1)

    val timeAfterFirstSort = System.currentTimeMillis()
    var res = timeAfterFirstSort - start
    println("MergeSort time for 10000 ints:\t" + res)

    list2 = mergeS.mergeSortParallel(list2, 4)
    res = System.currentTimeMillis() - timeAfterFirstSort
    println("ParallelMergeSort time for 10000 ints:\t" + res)

    assert(mergeS.isSorted(list1))
    assert(mergeS.isSorted(list2))
  }

  test ("Test finding fibonacci numbers: 25") {

    val start = System.currentTimeMillis
    val fibNormal = fibNumb.findFibNumber(25)

    val timeAfter = System.currentTimeMillis()
    var res = timeAfter - start
    println("Find Fibonacci Number time - 25:\t" + res)

    val fibParallel = fibNumb.findFibNumbParallel(25, 5)
    val timeAfter2 = System.currentTimeMillis()
    res = timeAfter2 - timeAfter
    println("Find Parallel Fibonacci Number time - 25:\t" + res)

    val fibTailRec = fibNumb.findFibNumbTailRec(25)
    res = System.currentTimeMillis() - timeAfter2
    println("Find Fibonacci Number (tail rec) time - 25:\t" + res)

    assert(fibNormal == 75025)
    assert(fibParallel == 75025)
    assert(fibTailRec == 75025)
  }

  test ("Test finding fibonacci numbers: 35") {

    val start = System.currentTimeMillis
    val fibNormal = fibNumb.findFibNumber(35)

    val timeAfter = System.currentTimeMillis()
    var res = timeAfter - start
    println("Find Fibonacci Number time - 35:\t" + res)

    val fibParallel = fibNumb.findFibNumbParallel(35, 5)
    val timeAfter2 = System.currentTimeMillis()
    res = timeAfter2 - timeAfter
    println("Find Parallel Fibonacci Number time - 35:\t" + res)

    val fibTailRec = fibNumb.findFibNumbTailRec(35)
    res = System.currentTimeMillis() - timeAfter2
    println("Find Fibonacci Number (tail rec) time - 35:\t" + res)

    assert(fibNormal == 9227465)
    assert(fibParallel == 9227465)
    assert(fibTailRec == 9227465)
  }

  test ("Test finding fibonacci numbers: 40") {

    val start = System.currentTimeMillis
    val fibNormal = fibNumb.findFibNumber(40)

    val timeAfter = System.currentTimeMillis()
    var res = timeAfter - start
    println("Find Fibonacci Number time - 40:\t" + res)

    val fibParallel = fibNumb.findFibNumbParallel(40, 5)
    val timeAfter2 = System.currentTimeMillis()
    res = timeAfter2 - timeAfter
    println("Find Parallel Fibonacci Number time - 40:\t" + res)

    val fibTailRec = fibNumb.findFibNumbTailRec(40)
    res = System.currentTimeMillis() - timeAfter2
    println("Find Fibonacci Number (tail rec) time - 40:\t" + res)

    assert(fibNormal == 102334155)
    assert(fibParallel == 102334155)
    assert(fibTailRec == 102334155)
  }

}
