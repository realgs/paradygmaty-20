package L6
import scala.util.Random

object TimeComparison {
  // helper method
  private def printTimeResults(startSequential: Long, endSequential: Long, startParallel: Long, endParallel: Long): Unit ={
    System.out.println("1) sequential version: " + (endSequential - startSequential) + " ns")
    System.out.println("2)  parallel  version: " + (endParallel - startParallel) + " ns")
  }

  def quickSortTimeComparison_1(): Unit = {
    System.out.println("\nTime comparison for QuickSort")
    System.out.println("*** Sorting array of 100000 elements ***")
    val array1 = Array.fill(10000){Random.nextDouble()}
    val array2 = new Array[Double](array1.size)
    Array.copy(array1, 0, array2, 0, array1.size)

    val t0 = System.nanoTime()
    QuickSort.quickSort(array1)
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    QuickSort.quickSortParallel(array2)
    val t3 = System.nanoTime()

    printTimeResults(t0, t1, t2, t3)
  }

  def quickSortTimeComparison_2(): Unit = {
    System.out.println("\nTime comparison for QuickSort")
    System.out.println("*** Sorting array of 1000000 elements ***")
    val array3 = Array.fill(1000000){Random.nextDouble()}
    val array4 = new Array[Double](array3.size)
    Array.copy(array3, 0, array4, 0, array3.size)

    val t4 = System.nanoTime()
    QuickSort.quickSort(array3)
    val t5 = System.nanoTime()

    val t6 = System.nanoTime()
    QuickSort.quickSortParallel(array4)
    val t7 = System.nanoTime()

    printTimeResults(t4, t5, t6, t7)
  }

  def numberOfElementsBTTimeComparison_1(): Unit = {
    System.out.println("\nTime comparison for function counting number of specific element in BT")

    System.out.println("*** BT which depth is 15 ***")
    val tree1 = BinaryTree.generateBT(15, 100)
    val t0 = System.nanoTime()
    BinaryTree.numberOfElementBT(tree1, 1)
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    BinaryTree.numberOfElementBTParallel(tree1, 1)
    val t3 = System.nanoTime()
    printTimeResults(t0, t1, t2, t3)
  }

  def numberOfElementsBTTimeComparison_2(): Unit = {
    System.out.println("\nTime comparison for function counting number of specific element in BT")

    System.out.println("*** BT which depth is 20 ***")
    val tree2 = BinaryTree.generateBT(20, 100)
    val t4 = System.nanoTime()
    BinaryTree.numberOfElementBT(tree2, 1)
    val t5 = System.nanoTime()

    val t6 = System.nanoTime()
    BinaryTree.numberOfElementBTParallel(tree2, 1)
    val t7 = System.nanoTime()

    printTimeResults(t4, t5, t6, t7)
  }

  def listProductTimeComparison_1(): Unit = {
    System.out.println("\nTime comparison for function counting product of elements in list")

    System.out.println("*** List of 10000 elements ***")
    val list1 = List.fill(10000) {Random.nextInt(100)}
    val t0 = System.nanoTime()
    ListProduct.listProduct(list1)
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    ListProduct.listProductParallel(list1)
    val t3 = System.nanoTime()
    printTimeResults(t0, t1, t2, t3)
  }

  def listProductTimeComparison_2(): Unit = {
    System.out.println("\nTime comparison for function counting product of elements in list")
    System.out.println("*** List of 100000 elements ***")
    val list2 = List.fill(100000) {Random.nextInt(100)}
    val t4 = System.nanoTime()
    ListProduct.listProduct(list2)
    val t5 = System.nanoTime()

    val t6 = System.nanoTime()
    ListProduct.listProductParallel(list2)
    val t7 = System.nanoTime()

    printTimeResults(t4, t5, t6, t7)
  }

    def areTheSameBTTimeComparison_1(): Unit = {
      System.out.println("\nTime comparison for function checking if BT are the same")
      System.out.println("*** BT which depth is 20 ***")
      val tree1 = BinaryTree.generateBT(20, 100)
      val t0 = System.nanoTime()
      BinaryTree.areTheSameBT(tree1, tree1)
      val t1 = System.nanoTime()

      val t2 = System.nanoTime()
      BinaryTree.areTheSameBTParallel(tree1, tree1)
      val t3 = System.nanoTime()
      printTimeResults(t0, t1, t2, t3)
    }

  def areTheSameBTTimeComparison_2(): Unit = {
    System.out.println("\nTime comparison for function checking if BT are the same")
    System.out.println("*** BT which depth is 25 ***")
    val tree1 = BinaryTree.generateBT(25, 100)
    val t0 = System.nanoTime()
    BinaryTree.areTheSameBT(tree1, tree1)
    val t1 = System.nanoTime()

    val t2 = System.nanoTime()
    BinaryTree.areTheSameBTParallel(tree1, tree1)
    val t3 = System.nanoTime()
    printTimeResults(t0, t1, t2, t3)
  }

  def main(args: Array[String]): Unit = {
    quickSortTimeComparison_1()
    quickSortTimeComparison_2()
    numberOfElementsBTTimeComparison_1()
    numberOfElementsBTTimeComparison_2()
    listProductTimeComparison_1()
    listProductTimeComparison_2()
    areTheSameBTTimeComparison_1()
    areTheSameBTTimeComparison_2()
  }
}

