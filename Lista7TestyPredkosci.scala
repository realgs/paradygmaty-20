import Funkcje._

object Lista7TestyPredkosci {

  def main(args:Array[String]):Unit={
    testMergeSort()
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

}
