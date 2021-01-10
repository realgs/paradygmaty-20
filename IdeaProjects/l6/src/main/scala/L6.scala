object L6 {

  def copyArray(array: Array[Int]): Array[Int]={
    var i=0
    val result=new Array[Int](array.size)
    while(i<array.size){
      result(i)=array(i)
      i+=1
    }
    result
  }

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
    System.out.println()
  }

  def main(args: Array[String])={
    var i=1000
    while(i<=1000000){
      System.out.println("---------------------------------")
      System.out.println(i+"-ELEMENTS:")
      System.out.println("---------------------------------")
      var a=Array.fill(i){scala.util.Random.nextInt(i)}
      System.out.print("QuickSort-")
      measureTime(QuickSort.quickSort(copyArray(a)))
      System.out.print("QuickSortParallel-")
      measureTime(QuickSort.quickSortParallel(copyArray(a)))
      System.out.print("BinarySearch-")
      measureTime(BinarySearch.contains(copyArray(a), -1))
      System.out.print("BinarySearchParallel-")
      measureTime(BinarySearch.containsParallel(copyArray(a), -1))
      System.out.println("---------------------------------")
      i*=10
    }
  }
}
