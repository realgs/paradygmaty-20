import scala.annotation.tailrec

object L6 {

  def measureTime[T](fun: =>T): Unit={
    val startTime=System.currentTimeMillis()
    fun
    val endTime=System.currentTimeMillis()
    System.out.println((endTime-startTime)+"ms")
  }

  def showArray(a: Array[Int]): Unit={
    var i=0;
    while(i<a.size){
      System.out.print(a(i)+" ")
      i+=1
    }
  }

  def main(args: Array[String])={
    var i=1000
    while(i<=1000000){
      System.out.println(i+"-elements:")
      var a=Array.fill(i){scala.util.Random.nextInt(i)}
      measureTime(QuickSort.quickSort(a))
      a=Array.fill(i){scala.util.Random.nextInt(i)}
      measureTime(QuickSortParallel.quickSortParallel(a))
      measureTime(BinarySearch.contains(a, -1))
      measureTime(BinarySearchParallel.containsParallel(a, -1))
      i*=10
    }
  }
}
