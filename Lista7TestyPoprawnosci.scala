import Funkcje._

object Lista7TestyPoprawnosci
{
  def main(args:Array[String]):Unit={
    testMergeSort()
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

}
