import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Random

object Funkcje {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def time[R](block: => R): Long = {

    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()

    t1 - t0
  }

  def generateListOfInts(n:Int):List[Int]={
    def helper(n:Int,result:List[Int]):List[Int]={
      if(n == 0) result
      else helper(n - 1,Random.nextInt(1000000)::result)
    }
    helper(n,Nil)
  }
  def generateListOfDoubles(n:Int):List[Double]={
    def helper(n:Int,result:List[Double]):List[Double]={
      if(n == 0) result
      else helper(n - 1,Random.nextDouble()*1000000::result)
    }
    helper(n,Nil)
  }

  // Merge sort
  private def merge(list1:List[Int],list2:List[Int],result:List[Int]):List[Int] =
    (list1,list2) match {
      case (Nil,Nil) => result
      case (l,Nil) => result.reverse:::l
      case (Nil,r) => result.reverse:::r
      case (h1::t1,h2::t2) => if(h1 <= h2) merge(t1,h2::t2,h1::result)
      else merge(h1::t1,t2,h2::result)
    }

  private def partition(list:List[Int],count:Int):(List[Int],List[Int])=
  {
    def helper(secondList:List[Int],firstList:List[Int],count:Int):(List[Int],List[Int])=
      (secondList,firstList,count) match{
        case (Nil,f,_) => (f.reverse,Nil)
        case (s,f,0) => (f.reverse,s)
        case (h::t,f,i) => helper(t,h::f,i-1)
      }
    helper(list,Nil,count)
  }

  def mergesort(list:List[Int]):List[Int] ={
    def mergeSort(subList:List[Int]):List[Int]={
      if(subList.size <= 1) subList
      else {
        val (firstList, secondList) = partition(subList, subList.size / 2)
        val left = mergeSort(firstList)
        val right = mergeSort(secondList)
        merge(left,right,Nil)
      }
    }
    if(list.size <= 1) list
    else
    {
      mergeSort(list)
    }
  }
  def mergesortParallel(list:List[Int]):List[Int] ={
    def mergeSort(subList:List[Int],actualSize:Int):List[Int]={
      if(subList.size <= 1) subList
      else {
        val (firstList, secondList) = partition(subList, subList.size / 2)

        if(actualSize >= 10000)
        {
          val f1 = Future {mergeSort(firstList, actualSize / 2)}
          val f2 = Future {mergeSort(secondList, actualSize / 2)}
          Await.result(for(first <- f1;second <- f2) yield merge(first,second,Nil),Duration.Inf)
        }
        else
        {
          val left = mergeSort(firstList, actualSize / 2)
          val right = mergeSort(secondList, actualSize / 2)
          merge(left,right,Nil)
        }
      }
    }
    if(list.size <= 1) list
    else
    {
      mergeSort(list,list.size)
    }
  }


  // Operations on list (Funkcja wykonująca funkcję z listy operations na list, i zwracająca wyniki w postaci listy)
  def doOperationsOnList(list:List[Double],operations: List[List[Double] => Double]):List[Double]={
    operations.foldLeft(Nil:List[Double])((result,operator) => operator(list)::result)
  }
  def doOperationsOnListParallel(list:List[Double],operations: List[List[Double] => Double]):List[Double]={
    def getListOfFutures(operations:List[List[Double] => Double]):List[Future[Double]] = {
      if(operations == Nil) Nil
      else Future{operations.head(list)}::getListOfFutures(operations.tail)
    }
    val futures = getListOfFutures(operations)

    val f = Future.sequence(futures)
    Await.result(f, Duration.Inf)
  }

  // Quick sort
  private def swap(tab:Array[Int])(i:Int)(j:Int):Unit= {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  private def choose_pivot(tab:Array[Int])(m:Int)(n:Int):Int= tab((m + n)/2)

  private def partition(tab:Array[Int])(l:Int)(r:Int):(Int,Int)={
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)

    while(i <= j){
      while(tab(i) < pivot) i += 1
      while(pivot < tab(j)) j -= 1

      if(i<=j) {swap(tab)(i)(j);i+=1;j-=1}
    }
    (i,j)
  }

  private def quick(tab:Array[Int])(l:Int)(r:Int):Unit={
    if(l<r){
      val (i,j) = partition(tab)(l)(r)
      if(j-l<r-i)
      {
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      }
      else
      {
        quick(tab)(i)(r)
        quick(tab)(l)(j)
      }
    }
  }

  private def quickParallel(tab:Array[Int])(l:Int)(r:Int):Unit={
    if(l<r){
      val (i,j) = partition(tab)(l)(r)
      if(j-l<r-i)
      {
        val f1 = Future{quick(tab)(l)(j)}
        val f2 = Future{quick(tab)(i)(r)}
        Await.result(f1,Duration.Inf)
        Await.result(f2,Duration.Inf)
      }
      else
      {
        val f1 = Future{quick(tab)(i)(r)}
        val f2 = Future{quick(tab)(l)(j)}
        Await.result(f1,Duration.Inf)
        Await.result(f2,Duration.Inf)
      }
    }
  }

  def quicksort(tab:Array[Int]):Array[Int] = {quick(tab)(0)(tab.length-1);tab}
  def quicksortParallel(tab:Array[Int]):Array[Int] = {quickParallel(tab)(0)(tab.length-1);tab}
}
