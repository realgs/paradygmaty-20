import Funkcje._

object Lista7TestyPredkosci {

  def main(args:Array[String]):Unit={
    //testMergeSort()
    testOperationsOnList()
  }

  // Merge sort w wersji parallel jest średnio od 1.3 do 1.6 razy bardziej wydajny od zwykłej wersji
  def testMergeSort(): Unit =
  {
    println("Results for 10000 data")
    sizeTestOfMergeSort(10000,10000)

    println("\nResults for 100000 data")
    sizeTestOfMergeSort(100000,1000)

    println("\nResults for 1000000 data")
    sizeTestOfMergeSort(1000000,100)

    println("\nResults for 5000000 data")
    sizeTestOfMergeSort(5000000,10)
  }

  private def sizeTestOfMergeSort(size:Int,numberOfExperiments:Int):Unit=
  {
    var normalSum = 0l
    var parallelSum = 0l

    for(_ <- 0 until numberOfExperiments)
    {
      val list = generateListOfInts(size)
      normalSum += time{mergesort(list)}
      parallelSum += time{mergesortParallel(list)}
    }

    val normalAverage = normalSum/numberOfExperiments
    val parallelAverage =parallelSum/numberOfExperiments

    println("Average time of mergesort for " + numberOfExperiments + " experiments is " + normalAverage +"ns")
    println("Average time of parallel mergesort for " + numberOfExperiments + " experiments is " + parallelAverage +"ns")
    println("Parallel version is " + normalAverage/parallelAverage.asInstanceOf[Double] + " times faster than normal version")
  }

  // OperationsOnList w wersji parallel jest od 1.3 razy bardziej wydajny(dla 10000 danych)
  // do 6(ilość operacji do wykonania) razy bardziej wydajny(dla 10000000 danych)
  // Wersja z równoległościa, działa im lepiej im więcej operacji wykonuje każdy z wątków
  def testOperationsOnList():Unit =
  {
    println("\nResults for 10000 data")
    sizeTestOfOperationsOnList(10000,100000)

    println("\nResults for 100000 data")
    sizeTestOfOperationsOnList(100000,10000)

    println("\nResults for 1000000 data")
    sizeTestOfOperationsOnList(1000000,1000)

    println("\nResults for 10000000 data")
    sizeTestOfOperationsOnList(10000000,100)
  }

  private def sizeTestOfOperationsOnList(size:Int,numberOfExperiments:Int):Unit=
  {
    val operations = List((list:List[Double]) => list.foldLeft(0d)((current:Double,result:Double) => current + result),
      (list:List[Double]) => list.foldLeft(0d)((result:Double,current:Double) => current - result),
      (list:List[Double]) => list.foldLeft(1d)((result:Double,current:Double) => current * result),
      (list:List[Double]) => list.foldLeft(1d)((result:Double,current:Double) => current / result),
      (list:List[Double]) => list.foldLeft(0d)((result:Double,current:Double) => current % result),
      (list:List[Double]) => list.foldLeft(0d)((result:Double,current:Double) => result / 2 + current * 3 + 5))

    var normalSum = 0l
    var parallelSum = 0l

    for(_ <- 0 until numberOfExperiments)
    {
      val list = generateListOfDoubles(size)
      normalSum += time{doOperationsOnList(list,operations)}
      parallelSum += time{doOperationsOnListParallel(list,operations)}
    }

    val normalAverage = normalSum/numberOfExperiments
    val parallelAverage = parallelSum/numberOfExperiments

    println("Average time of OperationsOnList for " + numberOfExperiments + " experiments is " + normalAverage +"ns")
    println("Average time of parallel OperationsOnList for " + numberOfExperiments + " experiments is " + parallelAverage +"ns")
    println("Parallel version is " + normalAverage/parallelAverage.asInstanceOf[Double] + " times faster than normal version")
  }
}
