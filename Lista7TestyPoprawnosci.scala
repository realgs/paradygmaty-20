import Funkcje._

object Lista7TestyPoprawnosci
{
  def main(args:Array[String]):Unit={
    testMergeSort()
    testOperationsOnList()
    testQuickSort()
  }

  def testMergeSort(): Unit =
  {
     val ascendSort = (a:Int,b:Int) => a < b

     val emptyList = List[Int]()
     val emptyListResult = emptyList.sortWith(ascendSort)

     val oneElementList = List(1)
     val oneElementListResult = oneElementList.sortWith(ascendSort)

     val smallList = List(10,9,8,7,6,5,4,3,2,1)
     val smallListResult = smallList.sortWith(ascendSort)

     val mediumList = generateListOfInts(10000)
     val mediumListResult = mediumList.sortWith(ascendSort)

     val bigList = generateListOfInts(1000000)
     val bigListResult = bigList.sortWith(ascendSort)

     println("Test poprawności mergesorta")
     println("Test1(pusta lista): " + (emptyListResult == mergesort(emptyList) && emptyListResult == mergesortParallel(emptyList)))
     println("Test2(jeden element): " + (oneElementListResult == mergesort(oneElementList) && oneElementListResult == mergesortParallel(oneElementList)))
     println("Test3(mała lista): " + (smallListResult == mergesort(smallList) && smallListResult == mergesortParallel(smallList)))
     println("Test4(średnia lista): " + (mediumListResult == mergesort(mediumList) && mediumListResult == mergesortParallel(mediumList)))
     println("Test5(duża lista): " + (bigListResult == mergesort(bigList) && bigListResult == mergesortParallel(bigList)))
  }

  def testOperationsOnList():Unit =
  {
    val operations = List((list:List[Double]) => list.foldLeft(0d)((current:Double,result:Double) => current + result),
      (list:List[Double]) => list.foldLeft(0d)((result:Double,current:Double) => current + result))

    val emptyList = List[Double]()
    val emptyListResult = emptyList.sum :: emptyList.sum :: Nil

    val oneElementList = List(1d)
    val oneElementListResult = oneElementList.sum :: oneElementList.sum :: Nil

    val smallList = generateListOfDoubles(100000)
    val smallListResult = smallList.sum :: smallList.sum :: Nil

    val mediumList = generateListOfDoubles(1000000)
    val mediumListResult = mediumList.sum :: mediumList.sum :: Nil

    val bigList = generateListOfDoubles(10000000)
    val bigListResult = bigList.sum :: bigList.sum :: Nil

    println("\nTest poprawności Operations on list")
    println("Test1(pusta lista): " + (emptyListResult == doOperationsOnList(emptyList,operations) &&
      emptyListResult == doOperationsOnListParallel(emptyList,operations)))
    println("Test2(jeden element): " + (oneElementListResult == doOperationsOnList(oneElementList,operations) &&
      oneElementListResult == doOperationsOnListParallel(oneElementList,operations)))
    println("Test3(mała lista): " + (smallListResult == doOperationsOnList(smallList,operations) &&
      smallListResult == doOperationsOnListParallel(smallList,operations)))
    println("Test4(średnia lista): " + (mediumListResult == doOperationsOnList(mediumList,operations) &&
      mediumListResult == doOperationsOnListParallel(mediumList,operations)))
    println("Test5(duża lista): " + (bigListResult == doOperationsOnList(bigList,operations) &&
      bigListResult == doOperationsOnListParallel(bigList,operations)))
  }

  def testQuickSort(): Unit =
  {
    val ascendSort = (a:Int,b:Int) => a < b

    val emptyList = List[Int]()
    val emptyListResult = emptyList.sortWith(ascendSort)

    val oneElementList = List(1)
    val oneElementListResult = oneElementList.sortWith(ascendSort)

    val smallList = List(10,9,8,7,6,5,4,3,2,1)
    val smallListResult = smallList.sortWith(ascendSort)

    val mediumList = generateListOfInts(10000)
    val mediumListResult = mediumList.sortWith(ascendSort)

    val bigList = generateListOfInts(1000000)
    val bigListResult = bigList.sortWith(ascendSort)

    println("\nTest poprawności quicksorta")
    println("Test1(pusta lista): " + (emptyListResult == quicksort(emptyList.toArray).toList && emptyListResult == quicksortParallel(emptyList.toArray).toList))
    println("Test2(jeden element): " + (oneElementListResult == quicksort(oneElementList.toArray).toList && oneElementListResult == quicksortParallel(oneElementList.toArray).toList))
    println("Test3(mała lista): " + (smallListResult == quicksort(smallList.toArray).toList && smallListResult == quicksortParallel(smallList.toArray).toList))
    println("Test4(średnia lista): " + (mediumListResult == quicksort(mediumList.toArray).toList && mediumListResult == quicksortParallel(mediumList.toArray).toList))
    println("Test5(duża lista): " + (bigListResult == quicksort(bigList.toArray).toList && bigListResult == quicksortParallel(bigList.toArray).toList))
  }
}
